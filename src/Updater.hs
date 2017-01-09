{-# LANGUAGE RebindableSyntax #-}
module Updater
    ( updater
    ) where

import           Control.Monad
import           Foundation
import           System.IO (withFile, IOMode(..))
import           System.FilePath
import           System.Directory
import           Updater.Delta.BsDiff

-- | Create the destination file from source file using the patch
updater :: String -- ^ source path
        -> String -- ^ destination path
        -> String -- ^ diff specifier
        -> IO ()
updater srcFile dstFile diffPath = do
    when (srcFile == dstFile) $ error "destination cannot be the same as source"
    dstAlreadyExist <- doesFileExist (toList dstFile)
    when dstAlreadyExist $ error $ toList ("destination " <> diffPath <> " already exist")

    bsDiff <- openDiff diffPath
    withFile (toList dstFile) WriteMode $ \dst ->
        applyDiff bsDiff (toList srcFile) dst
    return ()

