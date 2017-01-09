{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Updater
import Foundation

main :: IO ()
main = do
    args <- getArgs
    case args of
        [a,b,c] -> updater a b c
        _       -> error ("usage: <program> <file-to-patch> <destination-name> <patch-file>")
