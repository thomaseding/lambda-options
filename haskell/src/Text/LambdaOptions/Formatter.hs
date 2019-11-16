{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Safe #-}

-- | Provides a configurable way to format help options for textual presentation.
module Text.LambdaOptions.Formatter (
  FormatConfig(..),
  defaultFormatConfig,
  format,
  formatKeywords,
) where

import           Control.Monad
                  ( forM_, when, unless, void )
import qualified Control.Monad.State as State
import           Control.Monad.State
                  ( State )
import           Data.Function
                  ( on )
import           Data.List
                  ( sortBy )
import           Text.LambdaOptions.Keyword
                  ( Keyword(..) )

--------------------------------------------------------------------------------

-- | Formats the given string with the given configuration.
format :: FormatConfig -> String -> String
format config str = runFormatter config $ do
  emitString str
  void flushWord

-- | Formats the given keywords with the given configuration.
formatKeywords :: FormatConfig -> [Keyword] -> String
formatKeywords config = runFormatter config . mapM_ formatKeyword

--------------------------------------------------------------------------------

-- | User configuration for formatting.
data FormatConfig
  = FormatConfig
    { fmtMaxWidth :: Int
    }
  deriving (Show, Read, Eq, Ord)

-- | > FormatConfig { fmtMaxWidth = 80 }
defaultFormatConfig :: FormatConfig
defaultFormatConfig = FormatConfig
  { fmtMaxWidth = 80
  }

--------------------------------------------------------------------------------

data FormatterState
  = FormatterState
    { fmtConfig :: FormatConfig
    , fmtEmittedChars :: [Char]
    , fmtWord :: [Char]
    , fmtWidth :: Int
    , fmtIndentation :: Int
    }

type Formatter = State FormatterState

runFormatter :: FormatConfig -> Formatter () -> String
runFormatter config m = reverse $ fmtEmittedChars $ State.execState m FormatterState
  { fmtConfig = config
  , fmtEmittedChars = []
  , fmtWord = []
  , fmtWidth = 0
  , fmtIndentation = 0
  }

formatKeyword :: Keyword -> Formatter ()
formatKeyword kwd = do
  State.modify $ \st -> st
    { fmtWidth = 0
    }
  changeIndentation 0
  newLine True
  formatKeywordNames kwd
  formatKeywordArgText kwd
  formatKeywordText kwd
  void flushWord

isShort :: String -> Bool
isShort name
  | nameLen <= 1
    = True
  | nameLen /= 2
    = False
  | otherwise
    = c == '-' || c == '/'
  where
    nameLen = length name
    c = head name

formatKeywordNames :: Keyword -> Formatter ()
formatKeywordNames kwd = do
  let names = sortBy cmp $ kwNames kwd
      (mShortName, otherNames) = case names of
        name : rest -> case isShort name of
          True  -> (Just name, rest)
          False -> (Nothing, names)
        [] -> (Nothing, [])
      otherIdxs = [maybe 0 (const 1) mShortName ..] :: [Int]

  case mShortName of
    Nothing -> pure ()
    Just shortName -> do
      changeIndentation 1
      emitString shortName

  forM_ (zip otherIdxs otherNames) $ \(idx, name) -> do
    when (idx > 0) $ do
      emitChar ','
    changeIndentation 5
    emitString name

  where
    cmp n1 n2 = case (compare `on` length) n1 n2 of
      LT -> LT
      GT -> GT
      EQ -> compare n1 n2

formatKeywordArgText :: Keyword -> Formatter ()
formatKeywordArgText kwd = case kwArgText kwd of
  "" -> pure ()
  argTxt -> do
    void flushWord
    changeIndentation . succ =<< State.gets fmtWidth
    emitString argTxt

formatKeywordText :: Keyword -> Formatter()
formatKeywordText kwd = do
  void flushWord
  case kwText kwd of
    ""  -> pure ()
    txt -> do
      changeIndentation . succ =<< State.gets fmtWidth
      changeIndentation 29
      emitString txt

flushWord :: Formatter Bool
flushWord = do
  st <- State.get
  case fmtWord st of
    [] -> pure False
    word -> do
      let indentation = fmtIndentation st
          width = fmtWidth st
          wordLen = length word
          maxWidth = fmtMaxWidth $ fmtConfig st
      unless (width == indentation || wordLen + width <= maxWidth) $ do
        newLine False
      State.modify $ \s -> s
        { fmtEmittedChars = word ++ fmtEmittedChars s
        , fmtWidth = fmtWidth s + wordLen
        , fmtWord = []
        }
      pure True

changeIndentation :: Int -> Formatter ()
changeIndentation newAmount = do
  void flushWord
  State.modify $ \st -> st
    { fmtIndentation = newAmount
    }
  indent True

indent :: Bool -> Formatter ()
indent doFlushWord = do
  when doFlushWord $ do
    void flushWord
  st <- State.get
  let indentation = fmtIndentation st
      width = fmtWidth st
      amount = indentation - width
  case width > indentation of
    True  -> newLine True
    False -> State.modify $ \s -> s
      { fmtEmittedChars = replicate amount ' ' ++ fmtEmittedChars s
      , fmtWidth = indentation
      }

newLine :: Bool -> Formatter ()
newLine doFlushWord = do
  emittedChars <- State.gets fmtEmittedChars
  unless (null emittedChars) $ do
    State.modify $ \st -> st
      { fmtEmittedChars = '\n' : fmtEmittedChars st
      }
  State.modify $ \st -> st
    { fmtWidth = 0
    }
  indent doFlushWord

emitSpace :: Formatter ()
emitSpace = flushWord >>= \case
  False -> pure ()
  True  -> do
    st <- State.get
    let width = fmtWidth st
        maxWidth = fmtMaxWidth $ fmtConfig st
    case width < maxWidth of
      True -> State.modify $ \s -> s
        { fmtEmittedChars = ' ' : fmtEmittedChars st
        , fmtWidth = width + 1
        }
      False -> newLine True

emitChar :: Char -> Formatter ()
emitChar = \case
  ' '  -> emitSpace
  '\n' -> do
    void flushWord
    newLine False
  c -> State.modify $ \st -> st
    { fmtWord = c : fmtWord st
    }

emitString :: String -> Formatter ()
emitString = mapM_ emitChar

