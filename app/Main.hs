module Main (main) where

import ClassyPrelude
import Lib
import qualified Adapter.InMemory.Auth as MemAuth

main :: IO ()
main = do
  MemAuth.runTest
