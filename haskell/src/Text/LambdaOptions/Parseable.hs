{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Text.LambdaOptions.Parseable (
    Parseable(..),
) where


import Data.Word
import Text.Read
import Text.Read.Bounded


--------------------------------------------------------------------------------


-- | Class describing parseable values. Much like the 'Prelude.Read' class.
class Parseable a where
    -- | Given a sequence of strings, returns 'Nothing' and the number of strings consumed if the parse failed.
    -- Otherwise, return 'Just' the parsed value and the number of strings consumed.
    -- Element-wise, an entire string must be parsed in the sequence to be considered
    -- a successful parse.
    parse :: [String] -> (Maybe a, Int)


simpleParse :: (String -> Maybe a) -> [String] -> (Maybe a, Int)
simpleParse parser args = case args of
        [] -> (Nothing, 0)
        s : _ -> case parser s of
            Nothing -> (Nothing, 0)
            Just x -> (Just x, 1)


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


-- | Identity parser.
instance Parseable String where
    parse = simpleParse Just


-- | Parses a 'Float' using its 'Prelude.Read' instance.
instance Parseable Float where
    parse = simpleParse readMaybe


-- | Greedily parses a single argument or no argument. Never fails.
instance (Parseable a) => Parseable (Maybe a) where
    parse args = case parse args of
        (Nothing, n) -> (Just Nothing, n)
        (Just x, n) -> (Just $ Just x, n)



