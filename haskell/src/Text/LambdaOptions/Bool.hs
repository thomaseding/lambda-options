{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Bool newtype wrapper that can be pattern matched against for parsing.
module Text.LambdaOptions.Bool (
  Booly(..),
  BoolString(..),
  BoolChar(..),
  BoolNum(..),
  BoolCasing(..),
  readBooly,
) where

import Data.Char (toLower)
import Data.Data
import Data.Monoid (mconcat, Alt(..))
import Text.LambdaOptions.Parseable
import Text.Read (readMaybe)

--------------------------------------------------------------------------------

data BoolString
  = AllowString
  | DisallowString

data BoolChar
  = AllowChar
  | DisallowChar

data BoolNum
  = AllowBit
  | AllowNat
  | AllowInteger
  | DisallowNum

data BoolCasing
  = IgnoreCase
  | LowerAll
  | UpperAll
  | UpperHead

data Booly
    (string :: BoolString)
    (char :: BoolChar)
    (num :: BoolNum)
    (casing :: BoolCasing)
  = Booly { unBooly :: Bool }
  deriving (Typeable, Data, Show, Read, Eq, Ord)

class ReadBoolString (string :: BoolString) where
  readBoolString :: BoolCasingVal casing => String -> Maybe (Booly string char num casing)

instance ReadBoolString 'DisallowString where
  readBoolString _ = Nothing

instance ReadBoolString 'AllowString where
  readBoolString
    :: forall char num casing
    .  BoolCasingVal casing
    => String
    -> Maybe (Booly 'AllowString char num casing)
  readBoolString str = fmap Booly $ case boolCasingVal @casing of
    IgnoreCase -> case map toLower str of
      "true"  -> Just True
      "false" -> Just False
      _ -> Nothing
    LowerAll -> case str of
      "true"  -> Just True
      "false" -> Just False
      _ -> Nothing
    UpperAll -> case str of
      "TRUE"  -> Just True
      "FALSE" -> Just False
      _ -> Nothing
    UpperHead -> case str of
      "True"  -> Just True
      "False" -> Just False
      _ -> Nothing

class ReadBoolChar (char :: BoolChar) where
  readBoolChar :: BoolCasingVal casing => String -> Maybe (Booly string char num casing)

instance ReadBoolChar 'DisallowChar where
  readBoolChar _ = Nothing

instance ReadBoolChar 'AllowChar where
  readBoolChar
    :: forall string num casing
    .  BoolCasingVal casing
    => String
    -> Maybe (Booly string 'AllowChar num casing)
  readBoolChar str = fmap Booly $ case boolCasingVal @casing of
    IgnoreCase -> case map toLower str of
      "t" -> Just True
      "f" -> Just False
      _ -> Nothing
    LowerAll -> case str of
      "t" -> Just True
      "f" -> Just False
      _ -> Nothing
    UpperAll -> case str of
      "T" -> Just True
      "F" -> Just False
      _ -> Nothing
    UpperHead -> case str of
      "T" -> Just True
      "F" -> Just False
      _ -> Nothing

class ReadBoolNum num where
  readBoolNum :: String -> Maybe (Booly string char num casing)

instance ReadBoolNum 'DisallowNum where
  readBoolNum _ = Nothing

instance ReadBoolNum 'AllowBit where
  readBoolNum = \case
    "0" -> Just $ Booly False
    "1" -> Just $ Booly True
    _ -> Nothing

instance ReadBoolNum 'AllowNat where
  readBoolNum str = case readMaybe str of
    Nothing -> Nothing
    Just (n :: Integer) -> case n of
      0 -> Just $ Booly False
      _ -> case n < 0 of
        True  -> Nothing
        False -> Just $ Booly True

instance ReadBoolNum 'AllowInteger where
  readBoolNum str = case readMaybe str of
    Nothing -> Nothing
    Just (n :: Integer) -> case n of
      0 -> Just $ Booly False
      _ -> Just $ Booly True

class BoolCasingVal (casing :: BoolCasing) where
  boolCasingVal :: BoolCasing

instance BoolCasingVal 'IgnoreCase where
  boolCasingVal = IgnoreCase

instance BoolCasingVal 'LowerAll where
  boolCasingVal = LowerAll

instance BoolCasingVal 'UpperAll where
  boolCasingVal = UpperAll

instance BoolCasingVal 'UpperHead where
  boolCasingVal = UpperHead

readBooly
  :: (ReadBoolString string, ReadBoolChar char, ReadBoolNum num, BoolCasingVal casing)
  => String
  -> Maybe (Booly string char num casing)
readBooly s = getAlt $ mconcat [Alt $ r s | r <- [readBoolString, readBoolChar, readBoolNum]]

instance
  ( ReadBoolString string
  , ReadBoolChar char
  , ReadBoolNum num
  , BoolCasingVal casing)
  => Parseable (Booly string char num casing) where
  parse = simpleParse readBooly

