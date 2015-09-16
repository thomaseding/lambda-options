{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Text.LambdaOptions.Parseable (
    Parseable(..),
    simpleParse,
    repeatedParse,
) where


import Data.Word
import Text.Read
import Text.Read.Bounded


--------------------------------------------------------------------------------


-- | Class describing parseable values. Much like the 'Text.Read.Read' class.
class Parseable a where
    -- | Given a sequence of strings, returns 'Nothing' and the number of strings consumed if the parse failed.
    --
    -- Otherwise, return 'Just' the parsed value and the number of strings consumed.
    --
    -- Element-wise, an entire string must be parsed in the sequence to be considered a successful parse.
    parse :: [String] -> (Maybe a, Int)


-- | Turns a parser of a single string into a parser suitable for a `Parseable` instance.
--
-- Useful for implementing a `Parseable` for a type with a `Text.Read.Read` instance by supplying `Text.Read.readMaybe` to this function.
--
-- __Note:__ The string is /not/ tokenized in any way before being passed into the input parser.
simpleParse :: (String -> Maybe a) -> ([String] -> (Maybe a, Int))
simpleParse parser args = case args of
        [] -> (Nothing, 0)
        s : _ -> case parser s of
            Nothing -> (Nothing, 0)
            Just x -> (Just x, 1)


-- | Repeatedly applies `parse` the given number of times, accumulating the results.
--
-- Useful for implementing new parsers.
--
-- Example:
-- @
-- data Point = Point Float Float Float
--
-- instance Parseable Point where
--     parse strs = case repeatedParse 3 strs of
--         (Just [x,y,z], n) -> (Just (Point x y z), n)`
--         (Nothing, n) -> (Nothing, n)
-- @
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


parseBounded :: (ReadBounded a) => [String] -> (Maybe a, Int)
parseBounded = simpleParse $ \str -> case readBounded str of
    NoRead -> Nothing
    ClampedRead _ -> Nothing
    ExactRead x -> Just x


-- | Parses a 'Word' using its 'Text.Read.Bounded.ReadBounded' instance.
instance Parseable Word where
    parse = parseBounded


-- | Parses an 'Int' using its 'Text.Read.Bounded.ReadBounded' instance.
instance Parseable Int where
    parse = parseBounded


-- | Parses an 'Integer' using its 'Prelude.Read' instance.
instance Parseable Integer where
    parse = parseBounded


instance Parseable Char where
    parse strs = case strs of
        [c] : _ -> (Just c, 1)
        _ -> (Nothing, 0)


-- | Identity parser.
-- Ex: @parse "abc" == (Just "abc", 1)
instance Parseable String where
    parse = simpleParse Just


-- | Parses a 'Float' using its 'Prelude.Read' instance.
instance Parseable Float where
    parse = simpleParse readMaybe


-- | Greedily parses a single argument or no argument. Never fails.
-- Ex: @parse (words "5 6 7") == (Just [5,6,7], 3)@
instance (Parseable a) => Parseable (Maybe a) where
    parse args = case parse args of
        (Nothing, n) -> (Just Nothing, n)
        (Just x, n) -> (Just $ Just x, n)


instance Parseable () where
    parse _ = (Just (), 0)


instance (Parseable a, Parseable b) => Parseable (a,b) where
    parse = toPair . parse2Tuple


instance (Parseable a, Parseable b, Parseable c) => Parseable (a,b,c) where
    parse = toPair . parse3Tuple


toPair :: (a, b, c) -> (a, b)
toPair (a, b, _) = (a, b)


parse2Tuple :: (Parseable a, Parseable b) => [String] -> (Maybe (a,b), Int, [String])
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


parse3Tuple :: (Parseable a, Parseable b, Parseable c) => [String] -> (Maybe (a,b,c), Int, [String])
parse3Tuple ss = case parse2Tuple ss of
    (mt, nt, st) -> case mt of
        Nothing -> (Nothing, nt, st)
        Just (a, b) -> let
            (mc, nc) = parse st
            sc = drop nc st
            mTup = fmap (\c -> (a, b, c)) mc
            in (mTup, nt + nc, sc)



