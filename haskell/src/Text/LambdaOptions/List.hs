{-# LANGUAGE DeriveDataTypeable #-}

module Text.LambdaOptions.List (
    List(..),
) where


import Data.Data
import Text.LambdaOptions.Parseable


--------------------------------------------------------------------------------


internalError :: a
internalError = error "InternalError: Text.LambdaOptions.List"


--------------------------------------------------------------------------------


-- | A simple wrapper over @[a]@. Used to avoid overlapping instances for @Parseable [a]@ and @Parseable String@
newtype List a = List [a]
    deriving (Typeable, Data, Show, Read, Eq, Ord)


-- | Greedily parses arguments item-wise. Never fails.
instance (Parseable a) => Parseable (List a) where
    parse args = case parse args of
        (Just mx, n) -> case mx of
            Just x -> let
                rest = drop n args
                in case parse rest of
                    (Just (List xs), n') -> (Just $ List $ x : xs, n + n')
                    (Nothing, _) -> internalError
            Nothing -> (Just $ List [], n)
        (Nothing, _) -> internalError


