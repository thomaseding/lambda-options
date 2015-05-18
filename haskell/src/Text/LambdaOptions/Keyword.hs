{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.LambdaOptions.Keyword (
    Keyword(..),
    ToKeyword(..),
    kw,
    argText,
    text,
) where


import Data.Data
import Data.String


--------------------------------------------------------------------------------


-- | An option keyword, such as @"--help"@
data Keyword = Keyword {
    kwNames :: [String], -- ^ All the aliases for this keyword.
    kwArgText :: String, -- ^ Text to describe the arguments to the option given by this keyword.
    kwText :: String -- ^ Text to describe the function of the option given by this keyword.
} deriving (Typeable, Data, Show, Eq, Ord)


instance IsString Keyword where
    fromString name = kw [name]


-- | Convenience 'Keyword' creation class.
class ToKeyword a where
    toKeyword :: a -> Keyword


-- | Identiy mapping.
instance ToKeyword Keyword where
    toKeyword = id


-- | Used to create a 'Keyword' with a single alias.
instance ToKeyword String where
    toKeyword name = toKeyword [name]


-- | Used to create a 'Keyword' with many (or no) aliases
instance ToKeyword [String] where
    toKeyword names = Keyword {
        kwNames = names,
        kwArgText = "",
        kwText = "" }


-- | Shorthand for 'toKeyword'.
kw :: (ToKeyword a) => a -> Keyword
kw = toKeyword


-- | Sets the 'kwArgText' field in the keyword. Intended to be used infix:
--
-- > kw "--directory" `argText` "DIR" `text` "Write files to DIR."
argText :: Keyword -> String -> Keyword
argText k s = k { kwArgText = s }


-- | Sets the 'kwText' field in the keyword. Intended to be used infix.
--
-- > kw "--quiet" `text` "Suppress message display."
text :: Keyword -> String -> Keyword
text k s = k { kwText = s }




