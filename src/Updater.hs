{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Updater
    ( updater
    ) where

import           Control.Exception
import           Control.Monad
import           Foundation
import           Foundation.Collection -- (splitOn)
import           System.IO (withFile, IOMode(..))
import           System.FilePath
import           System.Directory
import           System.IO.Error (isAlreadyExistsError)
import           System.Exit
import           Updater.Delta.BsDiff
import           Crypto.Hash (hashlazy, SHA512, Digest)
import           Crypto.Random
import qualified Prelude
import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as UTF8

-- | a simple verison of create a "unique" directory
createUniqueDir :: String -> Int -> IO String
createUniqueDir prefix n = do
    let dstDir = prefix <> show n
    created <- catch (createDirectory (toList dstDir) >> return True)
                     (\e -> case e of
                                _ | isAlreadyExistsError e -> return False
                                  | otherwise              -> throwIO e)
    case created of
        True  -> pure dstDir
        False -> createUniqueDir prefix (n+1)

computeHashLBS :: L.ByteString -> String
computeHashLBS lbs = show $ hashSHA512 lbs
  where
    hashSHA512 :: L.ByteString -> Digest SHA512
    hashSHA512 = hashlazy

computeHash :: String -> IO String
computeHash filePath = computeHashLBS <$> L.readFile (toList filePath)

containDirectory :: [Char] -> Bool
containDirectory s = elem '/' s

type DiffDigest        = String
type SourceDigest      = String
type DestinationDigest = String

newtype Manifest = Manifest [(SourceDigest, (DestinationDigest, DiffDigest))]
    deriving (Show,Eq,Monoid)

-- redirect to stderr
warning s = putStrLn s

-- redirect to stderr & add a flag
debug s = putStrLn s

programError :: String -> IO a
programError s = putStrLn ("error: " <> s) >> exitFailure

-- originally it was a FilePath, but it's quite practical to not have
-- to create any file on the filesystem, so considering that (hopefully)
-- all the content of the archive fits in memory, which should be very likely
-- we keep everything live and automatically use that
type DiffContent = L.ByteString

processTarEntries :: Show e
                  => ([(DiffContent, DiffDigest)], Maybe Manifest)
                  -> Tar.Entries e
                  -> IO ([(DiffContent, DiffDigest)], Maybe Manifest)
processTarEntries r       Tar.Done              = pure r
processTarEntries _       (Tar.Fail e)          = error (toList $ show e)
processTarEntries (r, mf) (Tar.Next entry next)
    | containDirectory (Tar.entryPath entry) = do
        warning ("skipping entry " <> show (Tar.entryPath entry) <> " only expecting file without directory")
        processTarEntries (r,mf) next
    | otherwise = do
        case (Tar.entryPath entry, Tar.entryContent entry) of
            ("MANIFEST", Tar.NormalFile content _) -> do
                debug "found manifest"
                let manifestData = foldl parseManifest (Manifest [])
                                 $ (fmap (words . fromList . UTF8.toString) . UTF8.lines)
                                 $ content
                processTarEntries (r, Just manifestData) next
            (patches, Tar.NormalFile content _)
                | isSuffixOf ".bsdiff" patches -> do
                    let hash = computeHashLBS content
                    processTarEntries ((content, hash) : r, mf) next
            (_, entryContent) -> do
                warning ("skipping unknown file: " <> show (Tar.entryPath entry))
                processTarEntries (r, mf) next
  where
    words :: String -> [String]
    words = splitOn (== ' ')

    parseManifest :: Manifest -> [String] -> Manifest
    parseManifest r l =
        case l of
            [srcDigest,dstDigest,diffDigest] -> Manifest [(srcDigest,(dstDigest,diffDigest))] `mappend` r
            _                                -> r

-- | Format of patch
data Format = Format_BsDiff | Format_Tar
    deriving (Show,Eq)

getFormat :: String -> IO Format
getFormat diffPath = do
    bs <- withFile (toList diffPath) ReadMode $ \h -> B.hGet h 512
    case bs of
            -- flaky detection
        _ | B.isInfixOf "ustar" bs     -> return Format_Tar
          | B.isPrefixOf "BSDIFF40" bs -> return Format_BsDiff
          | otherwise                  -> error (Prelude.show bs)

-- | Create the destination file from source file using the patch
updater :: String -- ^ source path
        -> String -- ^ destination path
        -> String -- ^ diff specifier
        -> IO ()
updater srcFile dstFile diffPath = do
    when (srcFile == dstFile) $ programError "destination cannot be the same as source"
    dstAlreadyExist <- doesFileExist (toList dstFile)
    when dstAlreadyExist $ programError ("destination " <> dstFile <> " already exist")

    getFormat diffPath >>= updaterWith
  where
    -- A single BsDiff, then we behave just like bspatch
    updaterWith Format_BsDiff = do
        bsDiff <- openDiff diffPath
        withFile (toList dstFile) WriteMode $ \dst ->
            applyDiff bsDiff (toList srcFile) dst
        return ()
    -- TAR file, we open the whole things, expecting no directory
    -- and only files either:
    --
    -- * Called MANIFEST containing lines of 3 digest separated by spaces
    -- * contains a file ending in .bsdiff, containing a BSDIFF40 file.
    --
    -- iterating patching until there's no more patching to do.
    --
    -- If no patching is done, error is returned
    updaterWith Format_Tar = do
        tarEntries <- Tar.read <$> L.readFile (toList diffPath)
        (r, mmanifest) <- processTarEntries ([], Nothing) tarEntries
        case mmanifest of
            Nothing                  -> programError "no MANIFEST found in updating archive"
            Just (Manifest manifest) -> do
                srcHash <- computeHash srcFile
                startApply r manifest (srcHash, srcFile)
                putStrLn "SUCCESS"
                exitSuccess

    startApply patches manifest = loopApply 0
      where
        loopApply :: Int -> (String, String) -> IO ()
        loopApply iteration (srcHash, srcFile) = do
            case lookup srcHash manifest of
                Nothing
                    | iteration == 0 -> programError "no patching done"
                    | otherwise      -> renameFile (toList srcFile) (toList dstFile)
                Just (dstHashExpected, diffHash) -> do
                    let dstFile = srcFile <> "-" <> show iteration
                    fileAlreadyExist <- doesFileExist (toList dstFile)
                    when fileAlreadyExist $ programError ("destination path file " <> dstFile <> " already exist")

                    case revLookup diffHash patches of
                        Nothing -> programError ("iteration " <> show iteration <> ": cannot find patches with hash: " <> diffHash)
                        Just diffContent -> do
                            bsDiff <- openDiffLbs diffContent
                            withFile (toList dstFile) WriteMode $ \dst -> applyDiff bsDiff (toList srcFile) dst

                            -- make sure we have the expected result from the MANIFEST
                            dstHashActual <- computeHash dstFile
                            when (dstHashExpected /= dstHashActual) $ programError "expected destination digest is not what it is"

                            -- next step of iteration
                            loopApply (iteration+1) (dstHashExpected, dstFile)

    cleanupDir _ = return ()

    revLookup :: Eq s => s -> [(f, s)] -> Maybe f
    revLookup x l = fmap fst $ find ((==) x . snd) l
