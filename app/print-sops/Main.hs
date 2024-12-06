module Main (main) where

import Data.Yaml
import Sops

main :: IO ()
main = do
  yaml <- decodeFileThrow ".sops.yaml" :: IO SopsRules
  putStr (show yaml)
  return ()
