{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Class used for parsing command-line options.
module Text.LambdaOptions.Parseable (
  Parseable(..),
  maybeParse,
  boundedParse,
  repeatedParse,

  simpleParse, -- Deprecated
) where

import           Data.Proxy
                  ( Proxy(..)
                  )
import           GHC.TypeLits
                  ( KnownNat, SomeNat, natVal, someNatVal
                  , KnownSymbol, SomeSymbol, symbolVal, someSymbolVal
                  )
import           Text.Read
                  ( readMaybe
                  )
import           Text.Read.Bounded
                  ( BoundedRead(..), ReadBounded(..)
                  )

--------------------------------------------------------------------------------

-- | Class describing parseable values. Much like the 'Read' class.
class Parseable a where
  -- | Given a sequence of strings, `parse` returns 'Nothing' and the number
  -- of strings (not characters) consumed if the parse failed.
  -- Otherwise, `parse` returns 'Just' the parsed value and the number of
  -- strings consumed.
  --
  -- Element-wise, an entire string must be parsed in the sequence to be
  -- considered a successful parse.
  parse :: [String] -> (Maybe a, Int)

-- | Turns a parser of a single string into a parser suitable for a `Parseable` instance.
--
-- Useful for implementing a `Parseable` for a type with a `Read` instance by
-- supplying `readMaybe` to this function.
--
-- __Note:__
-- The string is /not/ tokenized in any way before being passed into the input parser.
maybeParse :: (String -> Maybe a) -> ([String] -> (Maybe a, Int))
maybeParse parser = \case
  [] -> (Nothing, 0)
  s : _ -> case parser s of
    Nothing -> (Nothing, 0)
    Just x -> (Just x, 1)

{-# DEPRECATED simpleParse "Use 'maybeParse' instead." #-}
-- | Deprecated: Use 'maybeParse' instead.
simpleParse :: (String -> Maybe a) -> ([String] -> (Maybe a, Int))
simpleParse = maybeParse

-- | Turns a parser of a single string into a parser suitable for a `Parseable` instance.
--
-- Useful for implementing a `Parseable` for a type with a `Text.Read.Bounded.ReadBounded` instance by
-- supplying `Text.Read.Bounded.readBounded` to this function.
--
-- __Note:__
-- The string is /not/ tokenized in any way before being passed into the input parser.
boundedParse :: (String -> BoundedRead a) -> ([String] -> (Maybe a, Int))
boundedParse parser = maybeParse $ \s -> case parser s of
  NoRead -> Nothing
  ClampedRead _ -> Nothing
  ExactRead x -> Just x

-- | Repeatedly applies `parse` the given number of times, accumulating the results.
--
-- Useful for implementing new parsers.
--
-- Example:
--
-- > data Point = Point Float Float Float
-- >
-- > instance Parseable Point where
-- >   parse args = case repeatedParse 3 args of
-- >     (Just [x,y,z], n) -> (Just (Point x y z), n)`
-- >     (Nothing, n) -> (Nothing, n)
repeatedParse :: (Parseable a) => Int -> [String] -> (Maybe [a], Int)
repeatedParse n = toPair . repeatedParse' n

repeatedParse' :: (Parseable a) => Int -> [String] -> (Maybe [a], Int, [String])
repeatedParse' n ss = case n <= 0 of
  True -> (Just [], 0, ss)
  False -> let
    (mx, nx) = parse ss
    sx = drop nx ss
    in case mx of
      Nothing -> (Nothing, nx, sx)
      Just x -> let
        (mxs, nxs, sxs) = repeatedParse' (n - 1) sx
        in (fmap (x :) mxs, nx + nxs, sxs)

-- | Parses a 'Word' using its 'Text.Read.Bounded.ReadBounded' instance.
instance Parseable Word where
  parse = boundedParse readBounded

-- | Parses an 'Int' using its 'Text.Read.Bounded.ReadBounded' instance.
instance Parseable Int where
  parse = boundedParse readBounded

-- | Parses an 'Integer' using its 'Read' instance.
instance Parseable Integer where
  parse = maybeParse readMaybe

-- | Parses a single character string.
instance Parseable Char where
  parse strs = case strs of
    [c] : _ -> (Just c, 1)
    _ -> (Nothing, 0)

-- | Identity parser.
--
-- > parse [""] == (Just "", 1)
-- > parse ["foo"] == (Just "foo", 1)
-- > parse [] == (Nothing, 0)
instance Parseable String where
  parse = maybeParse Just

-- | Parses a 'Float' using its 'Read' instance.
instance Parseable Float where
  parse = maybeParse readMaybe

-- | Parses an 'Double' using its 'Read' instance.
instance Parseable Double where
  parse = maybeParse readMaybe

-- | Greedily parses a single argument or no argument. Never fails.
instance (Parseable a) => Parseable (Maybe a) where
  parse args = case parse args of
    (Nothing, n) -> (Just Nothing, n)
    (Just x, n) -> (Just $ Just x, n)

-- | Parses a 'KnownNat' by matching its corresponding shown 'natVal'.
--
-- Ex:
-- > parse ["0"] == (Just (Proxy :: Proxy 0), 1)
-- > parse ["13"] == (Just (Proxy :: Proxy 13), 1)
-- > parse ["00"] == Nothing
-- > parse ["0xFF"] == Nothing
instance (KnownNat n) => Parseable (Proxy n) where
  parse = \case
    [] -> (Nothing, 0)
    str : _ -> case show (natVal nat) == str of
      True  -> (Just nat, 1)
      False -> (Nothing, 0)
    where
      nat = Proxy :: Proxy n

-- | Parses the exact string given by 'symbolVal'.
--
-- Ex:
-- > parse [""] == (Just (Proxy :: Proxy ""), 1)
-- > parse ["foo"] == (Just (Proxy :: Proxy "foo"), 1)
-- > parse ["foo"] == (Nothing :: Maybe (Proxy :: "bar"), 0)
-- > parse [] == (Nothing, 0)
instance (KnownSymbol s) => Parseable (Proxy s) where
  parse = \case
    [] -> (Nothing, 0)
    str : _ -> case symbolVal sym == str of
      True  -> (Just sym, 1)
      False -> (Nothing, 0)
    where
      sym = Proxy :: Proxy s

-- | Opaque identity parser for 'GHC.TypeLits.SymbolVal'.
--
-- Ex:
-- > parse [""] == (Just (SomeSymbol (Proxy :: Proxy "")), 1)
-- > parse ["foo"] == (Just (SomeSymbol (Proxy :: Proxy "foo")), 1)
-- > parse [] == (Nothing, 0)
instance Parseable SomeSymbol where
  parse = \case
    [] -> (Nothing, 0)
    str : _ -> (Just $ someSymbolVal str, 1)

-- | Parses a 'SomeNat' by matching its corresponding shown 'natVal'.
--
-- Ex:
-- > parse ["0"] == (Just (Proxy :: SomeNat (Proxy 0)), 1)
-- > parse ["13"] == (Just (Proxy :: SomeNat (Proxy 13)), 1)
-- > parse ["00"] == Nothing
-- > parse ["0xFF"] == Nothing
instance Parseable SomeNat where
  parse = \case
    [] -> (Nothing, 0)
    s : _ -> case readMaybe s of
      Nothing -> (Nothing, 0)
      Just n  -> case show n == s of
        False -> (Nothing, 0)
        True  -> case someNatVal n of
          Nothing  -> (Nothing, 0)
          j@Just{} -> (j, 1)

-- | Always succeeds and never consumes any input.
instance Parseable () where
  parse _ = (Just (), 0)

-- | Parses two values sequentially.
instance
  ( Parseable a
  , Parseable b)
  => Parseable (a, b) where
  parse = toPair . parse2Tuple

-- | Parses three values sequentially.
instance
  ( Parseable a
  , Parseable b
  , Parseable c)
  => Parseable (a,b,c) where
  parse = toPair . parse3Tuple

toPair :: (a, b, c) -> (a, b)
toPair (a, b, _) = (a, b)

parse2Tuple
  :: (Parseable a, Parseable b)
  => [String]
  -> (Maybe (a, b), Int, [String])
parse2Tuple ss = let
  (ma, na) = parse ss
  sa = drop na ss
  in case ma of
    Nothing -> (Nothing, na, sa)
    Just a -> let
      (mb, nb) = parse sa
      sb = drop nb sa
      mTup = fmap (\b -> (a, b)) mb
      in (mTup, na + nb, sb)

parse3Tuple
  :: (Parseable a, Parseable b, Parseable c)
  => [String]
  -> (Maybe (a, b, c), Int, [String])
parse3Tuple ss = case parse2Tuple ss of
  (mt, nt, st) -> case mt of
    Nothing -> (Nothing, nt, st)
    Just (a, b) -> let
      (mc, nc) = parse st
      sc = drop nc st
      mTup = fmap (\c -> (a, b, c)) mc
      in (mTup, nt + nc, sc)

