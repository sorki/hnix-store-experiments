{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
module Main where

import qualified Data.Text.IO
import qualified Data.ByteString.Char8
import qualified Data.Attoparsec.Text.Lazy

import qualified System.Environment

import qualified System.Nix.Build
import qualified System.Nix.Derivation
import qualified System.Nix.StorePath
import           System.Nix.Store.Remote

parseDerivation source = do
  contents <- Data.Text.IO.readFile source
  case Data.Attoparsec.Text.Lazy.parseOnly
    (System.Nix.Derivation.parseDerivation "/nix/store") contents of
      Left e -> error e
      Right drv -> return drv

main = System.Environment.getArgs >>= \case
    [filename] -> do
      case System.Nix.StorePath.parsePath "/nix/store" (Data.ByteString.Char8.pack filename) of
        Left e -> error e
        Right p -> do
          d <- parseDerivation filename
          out <- runStore $ buildDerivation p d System.Nix.Build.Normal
          print out
    _ -> error "No input derivation file"

