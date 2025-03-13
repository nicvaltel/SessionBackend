module Main (main) where

import ClassyPrelude
import Lib
import qualified Adapter.InMemory.Auth as MemAuth
import qualified Adapter.HTTP.Web.Main 

main :: IO ()
main = do
  -- Adapter.HTTP.Web.Main.main
  -- MemAuth.runTest
  Lib.runRoutine
