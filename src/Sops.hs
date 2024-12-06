{-# LANGUAGE OverloadedStrings #-}

module Sops (SopsRules (..), Rule (..), GpgPubKey, AgePubKey, Group (..)) where

import Control.Applicative (Alternative (empty))
import Data.Maybe (fromMaybe)
import Data.Yaml
  ( FromJSON (parseJSON),
    Value (Object),
    (.:),
    (.:?),
  )

---------------- Types -----------------------------

newtype SopsRules = SopsRules {creation_rules :: [Rule]} deriving (Show)

data Rule = Rule
  { path_regex :: String,
    key_groups :: [Group]
  }
  deriving (Show)

type GpgPubKey = String

type AgePubKey = String

data Group = Group
  { gpg :: [GpgPubKey],
    age :: [AgePubKey]
  }
  deriving (Show)

---------------- Functions --------------------------

---------------- FromJSON instaces ------------------

instance FromJSON Group where
  parseJSON (Object o) = Group <$> (fromMaybe [] <$> (o .:? "pgp")) <*> (fromMaybe [] <$> o .:? "age")
  parseJSON _ = empty

instance FromJSON Rule where
  parseJSON (Object o) = do
    path_regex_m <- o .:? "path_regex"
    let path = fromMaybe ".*" path_regex_m
    key_groups_m <- o .:? "key_groups"
    case key_groups_m of
      Just groups -> return (Rule path groups)
      Nothing -> do
        groups <- parseJSON (Object o)
        return (Rule path groups)
  parseJSON _ = empty

instance FromJSON SopsRules where
  parseJSON (Object o) = SopsRules <$> o .: "creation_rules"
  parseJSON _ = empty
