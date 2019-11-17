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

-- | 'Booly' data type used for fine control of 'Bool' parsers.
module Text.LambdaOptions.Bool (
  Booly(..),
  BoolWord(..),
  BoolLetter(..),
  BoolNumber(..),
  BoolCasing(..),
  ReadBoolWord(..),
  ReadBoolLetter(..),
  ReadBoolNumber(..),
  BoolCasingVal(..),
  readBooly,
) where

import           Data.Char
                  ( toLower )
import           Data.Data
                  ( Data, Typeable )
import           Data.Monoid
                  ( mconcat, Alt(Alt, getAlt) )
import           Text.LambdaOptions.Parseable
                  ( Parseable(parse), maybeParse )
import           Text.Read
                  ( readMaybe )

--------------------------------------------------------------------------------

-- | Controls word representation for 'Booly'.
data BoolWord
  -- | Disallow @"true"@ and @"false"@ word representations.
  = DisallowWord
  -- | Allow @"true"@ and @"false"@ word representations.
  | AllowWord

-- | Controls letter representation for 'Booly'.
data BoolLetter
  -- | Disallow @"t"@ and @"f"@ letter representations.
  = DisallowLetter
  -- | Allow @"t"@ and @"f"@ letter representations.
  | AllowLetter

-- | Controls number representation for 'Booly'.
data BoolNumber
  -- | Disallow number representations.
  = DisallowNumber
  -- | Allow @0@ and @1@ number representations.
  | AllowBit
  -- | Allow @N >= 0@ integer representations.
  -- @0@ maps to 'False'. @N > 0@ maps to 'True'.
  | AllowNatural
  -- | Allow any @N@ integer representation.
  -- @0@ maps to 'False'. @N /= 0@ maps to 'True'.
  | AllowInteger

-- | Controls required casing for 'Booly'.
data BoolCasing
  -- | Casing is completely ignored.
  = IgnoreCase
  -- | Either casing satisfies a parse.
  | OrCasing BoolCasing BoolCasing
  -- | Fully lowercase is required.
  | LowerAll
  -- | Fully uppercase is required.
  | UpperAll
  -- | The first letter must be uppercase. The rest must be lowercase.
  | UpperHead

-- | Data type used for parsing 'Bool' values with various schemes.
--
-- It can be useful to alias for this type:
--
-- > type B = Booly 'AllowWord 'DisallowLetter 'DisallowNumber 'LowerAll
-- >
-- > pattern B :: Bool -> B
-- > pattern B x = Booly x
-- >
-- > b :: B -> Bool
-- > b = unBooly
data Booly
    (w :: BoolWord)
    (l :: BoolLetter)
    (n :: BoolNumber)
    (c :: BoolCasing)
  = Booly { unBooly :: Bool }
  deriving (Typeable, Data, Show, Read, Eq, Ord)

-- | Turns a type-level 'BoolCasing' into a value-level one.
class BoolCasingVal (c :: BoolCasing) where
  boolCasingVal :: BoolCasing

instance
  ( BoolCasingVal a
  , BoolCasingVal b)
  => BoolCasingVal ('OrCasing a b) where
  boolCasingVal = OrCasing (boolCasingVal @a) (boolCasingVal @b)

instance BoolCasingVal 'IgnoreCase where
  boolCasingVal = IgnoreCase

instance BoolCasingVal 'LowerAll where
  boolCasingVal = LowerAll

instance BoolCasingVal 'UpperAll where
  boolCasingVal = UpperAll

instance BoolCasingVal 'UpperHead where
  boolCasingVal = UpperHead

-- | Parsers for the various 'BoolWord' type constructors.
class ReadBoolWord (w :: BoolWord) where
  readBoolWord :: BoolCasingVal c => String -> Maybe (Booly w l n c)

instance ReadBoolWord 'DisallowWord where
  readBoolWord _ = Nothing

instance ReadBoolWord 'AllowWord where
  readBoolWord
    :: forall l n c
    .  BoolCasingVal c
    => String
    -> Maybe (Booly 'AllowWord l n c)
  readBoolWord = fmap Booly . readBoolWord' (boolCasingVal @c)

readBoolWord' :: BoolCasing -> String -> Maybe Bool
readBoolWord' casing str = case casing of
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
  OrCasing x y -> case readBoolWord' x str of
    Just b  -> Just b
    Nothing -> readBoolWord' y str

-- | Parsers for the various 'BoolLetter' type constructors.
class ReadBoolLetter (l :: BoolLetter) where
  readBoolLetter :: BoolCasingVal c => String -> Maybe (Booly w l n c)

instance ReadBoolLetter 'DisallowLetter where
  readBoolLetter _ = Nothing

instance ReadBoolLetter 'AllowLetter where
  readBoolLetter
    :: forall w n c
    .  BoolCasingVal c
    => String
    -> Maybe (Booly w 'AllowLetter n c)
  readBoolLetter = fmap Booly . readBoolLetter' (boolCasingVal @c)

readBoolLetter' :: BoolCasing -> String -> Maybe Bool
readBoolLetter' casing str = case casing of
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
  OrCasing x y -> case readBoolLetter' x str of
    Just b  -> Just b
    Nothing -> readBoolLetter' y str

-- | Parsers for the various 'BoolNumber' type constructors.
class ReadBoolNumber (n :: BoolNumber) where
  readBoolNumber :: String -> Maybe (Booly w l n c)

instance ReadBoolNumber 'DisallowNumber where
  readBoolNumber _ = Nothing

instance ReadBoolNumber 'AllowBit where
  readBoolNumber = \case
    "0" -> Just $ Booly False
    "1" -> Just $ Booly True
    _ -> Nothing

instance ReadBoolNumber 'AllowNatural where
  readBoolNumber str = case readMaybe str of
    Nothing -> Nothing
    Just (n :: Integer) -> case n of
      0 -> Just $ Booly False
      _ -> case n < 0 of
        True  -> Nothing
        False -> Just $ Booly True

instance ReadBoolNumber 'AllowInteger where
  readBoolNumber str = case readMaybe str of
    Nothing -> Nothing
    Just (n :: Integer) -> case n of
      0 -> Just $ Booly False
      _ -> Just $ Booly True

-- | Reads a 'Booly' from a 'String'.
readBooly
  :: (ReadBoolWord w, ReadBoolLetter l, ReadBoolNumber n, BoolCasingVal c)
  => String
  -> Maybe (Booly w l n c)
readBooly s = getAlt $ mconcat
  [ Alt $ r s
  | r <- [readBoolWord, readBoolLetter, readBoolNumber]
  ]

-- | Parses a 'Booly' using 'readBooly'.
instance
  ( ReadBoolWord w
  , ReadBoolLetter l
  , ReadBoolNumber n
  , BoolCasingVal c)
  => Parseable (Booly w l n c) where
  parse = maybeParse readBooly

