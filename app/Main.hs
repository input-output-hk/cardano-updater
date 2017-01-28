{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Updater
import Foundation

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["file",a,b,c] -> updater Crash a b c
        ["dir",a,c]    -> updateDirInplace a c
        _              ->
          error "usage: cardano-updater file \
                \<file-to-patch> <destination-file> <patch-file>\n\
                \       cardano-updater dir \
                \<dir-to-patch> <patch-file>\n"
