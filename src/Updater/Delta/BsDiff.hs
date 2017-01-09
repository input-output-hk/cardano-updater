--
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Updater.Delta.BsDiff
    ( openDiff
    , applyDiff
    ) where

import           Control.Monad (when)
import           Control.Concurrent.MVar
import qualified Codec.Compression.BZip as BZip
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as BS
import qualified Data.ByteArray as B
import           System.IO (withFile, hClose, IOMode(..), Handle)
import           Data.Bits (shiftL, (.|.), (.&.), testBit)

import           Updater.Delta.Stream

import           Foundation hiding (Offset)
import           Foundation.Collection
import           Foundation.Foreign
import           Foundation.IO.FileMap
import qualified Prelude
-- import           Foundation.Compat.ByteString (toByteString)

toByteString :: UArray Word8 -> L.ByteString
toByteString = L.pack . toList

-- sum the first bs with the second one.
bsPlusPrefix :: L.ByteString -> L.ByteString -> (L.ByteString, L.ByteString)
bsPlusPrefix b1 b2
    | len1 > len2  = let (b1', b1ret) = L.splitAt len2 b1
                      in (L.pack (L.zipWith (+) b1' b2), b1ret)
    | len1 == len2 = (L.pack $ L.zipWith (+) b1 b2, L.empty)
    | otherwise    =
        let (b2', b2Append) = L.splitAt len1 b2
         in (L.pack (L.zipWith (+) b1 b2') `L.append` b2Append, L.empty)
  where
    len1 = L.length b1
    len2 = L.length b2


type LFilePath = [Char]

type Offset = Int64

offsetToInt :: Offset -> Int
offsetToInt ofs = Prelude.fromIntegral ofs

-- | BsDiff Header
data Header = Header !Offset !Offset !Offset
    deriving (Show,Eq)

-- | BsDiff Content Handle, ready for application
--
-- it contains 3 differents compressed streamf of data
data BsDiffHandle = BsDiffHandle
    { bsHeader     :: Header
    , controlBlock :: MVar L.ByteString
    , dataBlock    :: MVar L.ByteString
    , extraBlock   :: MVar L.ByteString
    }

onControl :: BsDiffHandle -> Stream L.ByteString a -> IO a
onControl bsDiffHandle f = modifyMVar (controlBlock bsDiffHandle) (returnOrError . runStream f)

onData :: BsDiffHandle -> Stream L.ByteString a -> IO a
onData bsDiffHandle f = modifyMVar (dataBlock bsDiffHandle) (returnOrError . runStream f)

onExtra :: BsDiffHandle -> Stream L.ByteString a -> IO a
onExtra bsDiffHandle f = modifyMVar (extraBlock bsDiffHandle) (returnOrError . runStream f)

returnOrError :: Either String (a, L.ByteString) -> IO (L.ByteString, a)
returnOrError (Right (r,s)) = return (s, r)
returnOrError (Left err) = error (toList err)

toI64 :: [Word8] -> Offset
toI64 [a,b,c,d,e,f,g,h] =
    let val = fi (h .&. 0x7f) 56 .|. fi g 48 .|. fi f 40 .|. fi e 32
          .|. fi d            24 .|. fi c 16 .|. fi b 8  .|. fromIntegral a
     in case testBit h 7 of
            True  -> -val
            False -> val
  where fi i n = fromIntegral i `shiftL` n
toI64 l                 = error $ toList ("cannot convert " <> show l <> " to I64")

getOffset :: Stream L.ByteString Offset
getOffset = (toI64 . L.unpack) <$> consume 8

optional f = (Just <$> f) <|> pure Nothing

-- | Open a bsdiff and create a ready to apply structure
openDiff :: String -> IO BsDiffHandle
openDiff fp =
    toResult . runStream toDiffHandle =<< L.readFile (toList fp)
  where
    toResult (Left err) = error (toList err)
    toResult (Right ((hdr, ctrl, dat, extra), _)) =
        BsDiffHandle <$> pure hdr
                     <*> newMVar (BZip.decompress ctrl)
                     <*> newMVar (BZip.decompress dat)
                     <*> newMVar (BZip.decompress extra)
    toDiffHandle = do
        magic <- getOffset
        -- magic number associated with the file format. In ASCII, "BSDIFF40"
        when (magic /= 0x3034464649445342) $ error (toList $ show magic)
        hdr@(Header ctrlLen dataLen _) <- Header <$> getOffset <*> getOffset <*> getOffset
        ctrl  <- consume ctrlLen
        dat   <- consume dataLen
        extra <- remaining
        return (hdr, ctrl, dat, extra)

-- | Apply a diff that is coming from a BsDiffHandle, to the @src@ and pipe everything in the @dst@ Handle.
applyDiff :: BsDiffHandle -> LFilePath -> Handle -> IO ()
applyDiff bsDiffH src dst = fileMapReadWith (fromString src) start
  where
    start :: UArray Word8 -> IO ()
    start old = loop 0 0
      where
        put h dat = L.hPut h dat

        loop :: Int64 -> Int64 -> IO ()
        loop oldPos newPos = do
            c <- onControl bsDiffH $ optional ((,,) <$> getOffset <*> getOffset <*> getOffset)
            case c of
                Nothing                  -> return ()
                Just (ctrl0,ctrl1,ctrl2) -> do
                    diff <- onData bsDiffH $ consume ctrl0

                    let viewOld        = toByteString $ take (offsetToInt ctrl0) $ drop (offsetToInt oldPos) $ old
                        (toWrite,next) = bsPlusPrefix diff viewOld

                    put dst toWrite

                    when (not (L.null next)) $ error "remaining not NULL"

                    ext <- onExtra bsDiffH $ consume ctrl1
                    put dst ext

                    loop (oldPos + ctrl0 + ctrl2) (newPos + ctrl0 + ctrl1)
