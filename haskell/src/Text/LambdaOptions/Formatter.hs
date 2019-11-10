{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Safe #-}

-- | Provides a configurable way to format help options for textual presentation.
module Text.LambdaOptions.Formatter (
    FormatConfig(..),
    defaultFormatConfig,
    format,
    formatKeywords,
) where


import Control.Monad.State
import Data.Function
import Data.List
import Text.LambdaOptions.Keyword


--------------------------------------------------------------------------------


-- | Formats the given string with the given configuration.
format :: FormatConfig -> String -> String
format config str = runFormatter config $ do
    emitString str
    _ <- flushWord
    return ()


-- | Formats the given keywords with the given configuration.
formatKeywords :: FormatConfig -> [Keyword] -> String
formatKeywords config = runFormatter config . mapM_ formatKeyword


--------------------------------------------------------------------------------


data KeywordStyle
  = ShortLong
  | --Collapsing
  | CustomKeywordNames ([String] -> String)


-- | User configuration for formatting.
data FormatConfig = FormatConfig {
    fmtMaxWidth :: Int,
    fmtKeywordStyle :: KeywordStyle
} deriving (Show, Read, Eq, Ord)


-- | > FormatConfig { fmtMaxWidth = 80 }
defaultFormatConfig :: FormatConfig
defaultFormatConfig = FormatConfig {
    fmtMaxWidth = 80 }


--------------------------------------------------------------------------------


data FormatterState = FormatterState {
    fmtConfig :: FormatConfig,
    fmtEmittedChars :: [Char],
    fmtWord :: [Char],
    fmtWidth :: Int,
    fmtIndentation :: Int
} deriving ()


type Formatter = State FormatterState


runFormatter :: FormatConfig -> Formatter () -> String
runFormatter config m = reverse $ fmtEmittedChars $ execState m $ FormatterState {
    fmtConfig = config,
    fmtEmittedChars = [],
    fmtWord = [],
    fmtWidth = 0,
    fmtIndentation = 0 }


formatKeyword :: Keyword -> Formatter ()
formatKeyword kwd = do
    modify $ \st -> st { fmtWidth = 0 }
    changeIndentation 0
    newLine True
    formatKeywordNames kwd
    formatKeywordArgText kwd
    formatKeywordText kwd
    _ <- flushWord
    return ()


isShort :: String -> Bool
isShort name
    | nameLen <= 1 = True
    | nameLen /= 2 = False
    | otherwise = c == '-' || c == '/'
    where
        nameLen = length name
        c = head name


data AbbreviationFold a = AbbreviationFold
  { abbrFront :: [a]
  , abbrFold :: [a]
  , abbrEnd :: [a] }


discoverAbbreviationFold2 :: (Eq a) => [a] -> [a] -> Maybe (AbbreviationFold a)
discoverAbbreviationFold2 full abbr =
  case length full > length abbr of
    False -> Nothing
    True -> let
      abbrs = inits abbr
      fulls = take (length abbrs) $ inits fulls
      mFolds = zipWith stripPrefix abbrs fulls
      mFold = join $ find isJust mFolds 
      in mFold


discoverAbbreviationFoldN :: (Ord a) => [[a]] -> Maybe (AbbreviationFold a)
discoverAbbreviationFoldN = \case
  [] -> Nothing
  [_] -> Nothing
  xs@(x:_:_) -> let
    xs' = sort xs
    mFolds = map (discoverAbbreviationFold2 x) xs'
    folds = catMaybes mFolds
    in case length folds == length mFolds of
      False -> False
      True -> case reverse folds of
        [] -> assert False Nothing
        revFolds@(fold : _) -> case {-#drop 1#-} inits fold == revFolds of
          False -> Nothing
          True -> Just fold


formatKeywordNames' :: Keyword -> KeywordStyle -> Formatter ()
formatKeywordNames' kwd = \case
    ShortLong -> do
        let names = sortBy cmp $ kwNames kwd
            (mShortName, otherNames) = case names of
                name : rest -> case isShort name of
                    True -> (Just name, rest)
                    False -> (Nothing, names)
                [] -> (Nothing, [])
            otherIdxs = [maybe 0 (const 1) mShortName ..] :: [Int]
        case mShortName of
            Nothing -> return ()
            Just shortName -> do
                changeIndentation 1
                emitString shortName
        forM_ (zip otherIdxs otherNames) $ \(idx, name) -> do
            when (idx > 0) $ emitChar ','
            changeIndentation 5
            emitString name
        where
            cmp n1 n2 = case (compare `on` length) n1 n2 of
                LT -> LT
                GT -> GT
                EQ -> compare n1 n2
    Collapsing -> case discoverAbbreviationFoldN kwds of
        Just fold
        let names = sort $ kwNames kwd


formatKeywordNames :: Keyword -> Formatter ()
formatKeywordNames kwd = gets fmtKeywordStyle >>= formatKeywordNames' kwd


formatKeywordArgText :: Keyword -> Formatter ()
formatKeywordArgText kwd = case kwArgText kwd of
    "" -> return ()
    argTxt -> do
        _ <- flushWord
        changeIndentation . succ =<< gets fmtWidth
        emitString argTxt


formatKeywordText :: Keyword -> Formatter()
formatKeywordText kwd = do
    _ <- flushWord
    case kwText kwd of
        "" -> return ()
        txt -> do
            changeIndentation . succ =<< gets fmtWidth
            changeIndentation 29
            emitString txt


flushWord :: Formatter Bool
flushWord = do
    st <- get
    case fmtWord st of
        [] -> return False
        word -> do
            let indentation = fmtIndentation st
                width = fmtWidth st
                wordLen = length word
                maxWidth = fmtMaxWidth $ fmtConfig st
            unless (width == indentation || wordLen + width <= maxWidth) $ newLine False
            modify $ \s -> s {
                fmtEmittedChars = word ++ fmtEmittedChars s,
                fmtWidth = fmtWidth s + wordLen,
                fmtWord = "" }
            return True


changeIndentation :: Int -> Formatter ()
changeIndentation newAmount = do
    _ <- flushWord
    modify $ \st -> st { fmtIndentation = newAmount }
    indent True


indent :: Bool -> Formatter ()
indent doFlushWord = do
    when doFlushWord $ flushWord >> return ()
    st <- get
    let indentation = fmtIndentation st
        width = fmtWidth st
        amount = indentation - width
    case width > indentation of
        True -> newLine True
        False -> modify $ \s -> s {
            fmtEmittedChars = replicate amount ' ' ++ fmtEmittedChars s,
            fmtWidth = indentation }


newLine :: Bool -> Formatter ()
newLine doFlushWord = do
    emittedChars <- gets fmtEmittedChars
    unless (null emittedChars) $ modify $ \st -> st {
        fmtEmittedChars = '\n' : fmtEmittedChars st }
    modify $ \st -> st {
        fmtWidth = 0 }
    indent doFlushWord


emitSpace :: Formatter ()
emitSpace = flushWord >>= \case
    False -> return ()
    True -> do
        st <- get
        let width = fmtWidth st
            maxWidth = fmtMaxWidth $ fmtConfig st
        case width < maxWidth of
            True -> modify $ \s -> s {
                fmtEmittedChars = ' ' : fmtEmittedChars st,
                fmtWidth = width + 1 }
            False -> newLine True


emitChar :: Char -> Formatter ()
emitChar = \case
    ' ' -> emitSpace
    '\n' -> do
        _ <- flushWord
        newLine False
    c -> modify $ \st -> st {
        fmtWord = c : fmtWord st }


emitString :: String -> Formatter ()
emitString = mapM_ emitChar


