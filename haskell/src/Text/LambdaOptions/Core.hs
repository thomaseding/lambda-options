{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Contains the core functionality for LambdaOptions.
module Text.LambdaOptions.Core (
  runOptions,
  Options,
  OptionsM,

  OptionsError(..),
  prettyOptionsError,

  OptionCallback,
  addOption,

  getHelpDescription,

  getKeywords,
) where

import           Control.Monad
                  ( forM_ )
import qualified Control.Monad.State as State
import           Control.Monad.State
                  ( MonadState, State )
import           Data.Function
                  ( on )
import           Data.List
                  ( nub, sort, sortBy )
import qualified Data.Map as Map
import           Data.Map
                  ( Map )
import           Data.Maybe
                  ( mapMaybe )
import           Data.Proxy
                  ( Proxy(Proxy) )
import           Data.Typeable
                  ( TypeRep )
import           Text.LambdaOptions.Formatter
                  ( FormatConfig, defaultFormatConfig, formatKeywords )
import           Text.LambdaOptions.Internal.Opaque
                  ( Opaque, OpaqueCallback )
import           Text.LambdaOptions.Internal.OpaqueParser
                  ( OpaqueParser, GetOpaqueParsers, getOpaqueParsers )
import           Text.LambdaOptions.Internal.Wrap
                  ( Wrap, wrap )
import           Text.LambdaOptions.Keyword
                  ( Keyword(..) )
import           Text.LambdaOptions.Parseable
                  ( )

--------------------------------------------------------------------------------

-- | Describes the callback @f@ to be called for a successfully parsed option.
--
-- The function (or value) @f@ can have any arity and ultimately returns
-- a value with type @r@
--
-- Each of the callback's arguments must have a type @t@ which implements
-- 'Text.LambdaOptions.Parseable.Parseable' and 'Data.Typeable.Typeable'.
--
-- Think of this as the following constraint synonym:
--
-- > type OptionCallback r f = (f ~ ((Parseable t*, Typeable t*) => t0 -> t1 -> ... -> tN -> r))
--
-- Example callbacks:
--
-- > f0 = putStrLn "Option parsed!" :: IO ()
-- > f1 = put :: String -> State String ()
-- > f2 = liftIO . print :: (MonadIO m) => Int -> m ()
-- > f3 name year ratio = lift (print (name, year, ratio)) :: (MonadTrans m) => String -> Int -> Float -> m IO ()
-- > f4 = 7 :: Int
-- > f5 = (:) :: Double -> [Double] -> [Double]
type OptionCallback r f = (GetOpaqueParsers r f, Wrap r f)

internalizeKeyword :: Keyword -> Keyword
internalizeKeyword k = k
  { kwNames = nub $ sort $ kwNames k
  }

--------------------------------------------------------------------------------

data OptionInfo r
  = OptionInfo
    { optionKeyword :: Keyword
    , optionTypeReps :: [TypeRep]
    , optionOpaqueCallback :: OpaqueCallback r
    }

--------------------------------------------------------------------------------

-- | A monad for parsing options.
newtype OptionsM r a
  = Options
    { unOptions :: State (OptionsState r) a
    }

-- | A context for parsing options.
type Options r = OptionsM r ()

instance Functor (OptionsM r) where
  fmap f = Options . fmap f . unOptions

instance Applicative (OptionsM r) where
  pure = Options . pure
  Options f <*> Options x = Options (f <*> x)

instance Monad (OptionsM r) where
  return = Options . return
  Options x >>= f = Options (x >>= unOptions . f)

instance MonadState (OptionsState r) (OptionsM r) where
  get = Options State.get
  put = Options . State.put
  state = Options . State.state

data OptionsState r
  = OptionsState
    { stateOpaqueParsers :: Map TypeRep OpaqueParser
    , stateOptionsByArity :: [[OptionInfo r]]
    , stateCollectedActions :: [r]
    , stateCurrMark :: Int
    , stateHighMark :: Int
    , stateArgs :: [String]
    , stateFormatConfig :: FormatConfig
    }

{-# DEPRECATED parseFailedMessage "Use 'prettyOptionsError' instead." #-}

-- | Contains information about what went wrong during an unsuccessful options parse.
data OptionsError
  = ParseFailed
    { parseFailedMessage :: String
    , parseFailedArgs :: [String]
    , parseFailedBeginArgsIndex :: Int
    , parseFailedEndArgsIndex :: Int
    }
  deriving (Show)

-- | Pretty prints an 'OptionsError'.
prettyOptionsError :: OptionsError -> String
prettyOptionsError = \case
  e@ParseFailed
    { parseFailedArgs = args
    , parseFailedBeginArgsIndex = beginIndex
    , parseFailedEndArgsIndex = endIndex
    } -> mkParseFailedMessage beginIndex endIndex args

mkParseFailed :: Int -> Int -> [String] -> OptionsError
mkParseFailed beginIndex endIndex args = ParseFailed
  { parseFailedMessage = mkParseFailedMessage beginIndex endIndex args
  , parseFailedArgs = args
  , parseFailedBeginArgsIndex = beginIndex
  , parseFailedEndArgsIndex = endIndex
  }

mkParseFailedMessage :: Int -> Int -> [String] -> String
mkParseFailedMessage beginIndex endIndex args
  | endIndex == beginIndex + 1
    = "Unknown option at index " ++ beginIndexStr ++ ": `" ++ begin ++ "'"
  | endIndex == length args + 1
    = "Bad input for `" ++ begin ++ "' at index " ++ beginIndexStr ++ ": End of input."
  | otherwise
    = "Bad input for `" ++ begin ++ "' at index " ++ beginIndexStr ++ ": `" ++ end ++ "'"
  where
    begin = args !! beginIndex
    end = args !! (endIndex - 1)
    beginIndexStr = show beginIndex

-- | Tries to parse the supplied options against input arguments.
-- If successful, parsed option callback results are returned in 'Right'.
-- Otherwise 'OptionsError' is returned in 'Left'.
--
-- Example program:
--
-- > import qualified System.Environment as Env
-- > import qualified Text.LambdaOptions as L
-- >
-- > options :: L.Options (IO ())
-- > options = do
-- >
-- >   L.addOption
-- >     (L.kw ["--help", "-h"]
-- >     `L.text` "Display this help text.")
-- >     $ do
-- >       putStrLn "Usage:"
-- >       putStrLn $ L.getHelpDescription options
-- >
-- >   L.addOption
-- >     (L.kw "--add"
-- >     `L.argText` "X Y"
-- >     `L.text` "Adds two Doubles and prints their sum.")
-- >     $ \x y -> do
-- >       print $ x + (y :: Double)
-- >
-- > main :: IO ()
-- > main = do
-- >   args <- Env.getArgs
-- >   case L.runOptions options args of
-- >     Left e -> do
-- >       putStrLn $ L.prettyOptionsError e
-- >       putStrLn $ L.getHelpDescription options
-- >     Right results -> do
-- >       sequence_ results
--
-- >>> :main --add 3 0.14
-- 3.14
-- >>> :main -h
-- Usage:
--      --add X Y               Adds two Doubles and prints their sum.
--  -h, --help                  Display this help text.
-- >>> :main --add 0 1 --add 2 four
-- Bad input for `--add' at index 3: `four'
--      --add X Y               Adds two Doubles and prints their sum.
--  -h, --help                  Display this help text.
runOptions :: Options r -> [String] -> Either OptionsError [r]
runOptions action args = runOptions' args $ do
  runOptionsInternal defaultFormatConfig args $ do
    action
    tryParseAll

runOptionsInternal :: FormatConfig -> [String] -> OptionsM r a -> (a, OptionsState r)
runOptionsInternal config args action = State.runState (unOptions action) OptionsState
  { stateOpaqueParsers = Map.empty
  , stateOptionsByArity = []
  , stateCollectedActions = []
  , stateCurrMark = 0
  , stateHighMark = 0
  , stateArgs = args
  , stateFormatConfig = config
  }

runOptions' :: [String] -> (Bool, OptionsState r) -> Either OptionsError [r]
runOptions' args = \case
  (True , st) -> Right $ reverse $ stateCollectedActions st
  (False, st) -> Left $ let
    currMark = stateCurrMark st
    highMark = stateHighMark st
    in mkParseFailed currMark (highMark + 1) args

addByArity
  :: a -- Function
  -> [[a]] -- Functions grouped by arity
  -> Int -- Arity of input function
  -> [[a]]
addByArity x xss = \case
  0 -> case xss of
    [] -> [[x]]
    xs : rest -> (x : xs) : rest
  n -> case xss of
    [] -> [] : addByArity x [] (n - 1)
    xs : rest -> xs : addByArity x rest (n - 1)

-- | Adds the supplied option to the @Options r@ context.
--
-- If the keyword is matched and the types of the callback's parameters can
-- successfully be parsed, the callback is called with the parsed arguments.
addOption :: forall r f. (OptionCallback r f) => Keyword -> f -> Options r
addOption inKwd f = do
  let (reps, opaqueParsers) = unzip $ getOpaqueParsers (Proxy :: Proxy r) (Proxy :: Proxy f)
      arity = length reps
      f' = wrap f
      kwd = internalizeKeyword inKwd
      info = OptionInfo
        { optionKeyword = kwd
        , optionTypeReps = reps
        , optionOpaqueCallback = f'
        }

  forM_ (zip reps opaqueParsers) $ \(rep, opaqueParser) -> do
    State.modify $ \st -> st
      { stateOpaqueParsers = Map.insert rep opaqueParser $ stateOpaqueParsers st
      }

  State.modify $ \st -> st
    { stateOptionsByArity = addByArity info (stateOptionsByArity st) arity
    }

firstM :: (Monad m) => [m Bool] -> m Bool
firstM = \case
  m : ms -> m >>= \case
    False -> firstM ms
    True  -> pure True
  [] -> pure False

whileM :: (Monad m) => m Bool -> m ()
whileM m = m >>= \case
  True  -> whileM m
  False -> pure ()

tryParseAll :: OptionsM r Bool
tryParseAll = do
  whileM tryParse
  State.gets (null . stateArgs)

tryParse :: OptionsM r Bool
tryParse = State.gets (null . stateArgs) >>= \case
  True  -> pure False
  False -> tryParseByArity

hasEmptyKeyword :: OptionInfo r -> Bool
hasEmptyKeyword = null . kwNames . optionKeyword

tryParseByArity :: OptionsM r Bool
tryParseByArity = do
  optionsByArity <- State.gets $ reverse . stateOptionsByArity
  let filterEmpty f = map (filter $ f . hasEmptyKeyword) optionsByArity
      nonEmpties = filterEmpty not
      empties    = filterEmpty id
      goParse    = firstM . map tryParseByOptions
  goParse nonEmpties >>= \case
    True  -> pure True
    False -> goParse empties

tryParseByOptions :: [OptionInfo r] -> OptionsM r Bool
tryParseByOptions = firstM . map tryParseByOption

tryParseByOption :: OptionInfo r -> OptionsM r Bool
tryParseByOption option = do
  restorePoint <- State.get
  matchKeyword (optionKeyword option) >>= \case
    False -> pure False
    True  -> do
      let knownParsers = stateOpaqueParsers restorePoint
      args <- State.gets stateArgs
      beginMark <- State.gets stateCurrMark
      let reps = optionTypeReps option
          opaqueParsers = mapMaybe (flip Map.lookup knownParsers) reps
          (mOpaques, n) = sequenceParsers args opaqueParsers
          args' = drop n args
      result <- case mOpaques of
        Nothing -> do
          State.put restorePoint
          pure False
        Just opaques -> do
          let action = optionOpaqueCallback option opaques
          State.modify $ \st -> st {
            stateCurrMark = beginMark + n,
            stateCollectedActions = action : stateCollectedActions st,
            stateArgs = args' }
          pure True
      State.modify $ \st -> let
        oldHighMark = stateHighMark st
        newHighMark = max oldHighMark (beginMark + n)
        in st { stateHighMark = newHighMark }
      pure result

matchKeyword :: Keyword -> OptionsM r Bool
matchKeyword kwd = State.gets stateArgs >>= \args ->
  case kwNames kwd of
    [] -> pure True
    _  -> case args of
      [] -> pure False
      (arg : rest) -> case matchKeyword' arg kwd of
        Nothing -> pure False
        Just n -> do
          State.modify $ \st -> let
            newCurrMark = stateCurrMark st + n
            in st
              { stateCurrMark = newCurrMark
              , stateHighMark = max newCurrMark $ stateHighMark st
              , stateArgs = rest
              }
          pure True

matchKeyword' :: String -> Keyword -> Maybe Int
matchKeyword' arg kwd = case kwNames kwd of
  names -> case any (arg ==) names of
    False -> Nothing
    True  -> Just 1

sequenceParsers :: [String] -> [OpaqueParser] -> (Maybe [Opaque], Int)
sequenceParsers args = \case
  [] -> (Just [], 0)
  p : ps -> case p args of
    (Nothing, n) -> (Nothing, n)
    (Just o , n) -> let
      rest = drop n args
      in case sequenceParsers rest ps of
        (Nothing, n') -> (Nothing, n + n')
        (Just os, n') -> (Just $ o : os, n + n')

collectKeywords :: OptionsM r [Keyword]
collectKeywords = State.gets $ sortBy cmp . map optionKeyword . concat . stateOptionsByArity
  where
    cmp = namesCmp `on` kwNames
    namesCmp [] [] = EQ
    namesCmp [] _ = LT
    namesCmp _ [] = GT
    namesCmp ns1 ns2 = (compare `on` head) ns1 ns2

--------------------------------------------------------------------------------

createHelpDescription :: OptionsM r String
createHelpDescription = do
  config <- State.gets stateFormatConfig
  kwds <- collectKeywords
  pure $ formatKeywords config kwds

-- | Produces the help description given by the input options.
getHelpDescription :: Options r -> String
getHelpDescription options = fst $ runOptionsInternal defaultFormatConfig [] $ do
  options
  createHelpDescription

--------------------------------------------------------------------------------

-- | Produces the `Keyword`s inserted into the input options.
getKeywords :: Options r -> [Keyword]
getKeywords options = fst $ runOptionsInternal defaultFormatConfig [] $ do
  options
  collectKeywords

