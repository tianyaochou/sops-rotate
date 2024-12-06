module Main (main) where

import Data.ByteString.Char8
import Data.Maybe
import Data.Yaml
import Regex.RE2
import Sops
import System.Directory
import System.Process

listRecursively :: FilePath -> IO [FilePath]
listRecursively dir = do
  testDir <- doesDirectoryExist dir
  isSymlink <- pathIsSymbolicLink dir
  if not testDir
    then return [dir]
    else
      if isSymlink
        then return []
        else do
          items <- listDirectory dir
          let relative_paths = fmap ((dir ++ "/") ++) items
          mconcat (fmap listRecursively relative_paths)

matchPattern :: Pattern -> String -> Bool
matchPattern p s = isJust $ Regex.RE2.find p (pack s)

matchRegex :: String -> String -> Bool
matchRegex regex s = case compile (pack regex) of
  Left _ -> False
  Right p -> matchPattern p s

concatSep :: [a] -> [[a]] -> [a]
concatSep _ [] = []
concatSep sep (h : t) = h ++ sep ++ concatSep sep t

rotate :: FilePath -> IO ()
rotate p = callCommand $ concatSep " " ["sops", "rotate", "-i", p]

isSopsSecret :: [String] -> FilePath -> Bool
isSopsSecret patterns path = Prelude.any (`matchRegex` path) patterns

main :: IO ()
main = do
  paths <- listRecursively "."
  yaml <- decodeFileThrow ".sops.yaml" :: IO SopsRules
  let paths_regex = Prelude.map path_regex (creation_rules yaml)
  mconcat (rotate <$> Prelude.filter (isSopsSecret paths_regex) paths)

