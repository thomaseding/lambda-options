{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}


module Text.LambdaOptions (
    Options,
    Keyword(..),
    OptionCallback,
    addOption,
    HelpDescription(..),

    ToKeyword,
    kw,

    OptionsError(..),
    runOptions,

    Parseable(..),
    List(..),
) where


import Control.Applicative
import Control.Monad.Loops
import Control.Monad.State
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String
import Data.Maybe
import Data.Proxy
import Data.Typeable hiding (typeRep)
import Text.Read (readMaybe)


--------------------------------------------------------------------------------


internalError :: a
internalError = error "Internal logic error."


mkProxy :: a -> Proxy a
mkProxy _ = Proxy


--------------------------------------------------------------------------------


-- | A simple wrapper over @[a]@. Used to avoid overlapping instances for @Parseable [a]@ and @Parseable String@
newtype List a = List [a]
    deriving (Show, Read, Eq, Ord, Typeable)


--------------------------------------------------------------------------------


-- | When used as a callback argument, this contains the help description given by the added options.
--
-- Example:
--
-- > addOption (kw ["--help", "-h"]) $ \(HelpDescription desc) -> do
-- >     putStrLn desc
newtype HelpDescription = HelpDescription String
    deriving (Typeable)


--------------------------------------------------------------------------------


-- | Class describing parseable values. Much like the 'Prelude.Read' class.
class Parseable a where
    -- | Given a sequence of strings, returns 'Nothing' and the number of strings consumed if the parse failed.
    -- Otherwise, return 'Just' the parsed value and the number of strings consumed.
    -- Element-wise, an entire string must be parsed in the sequence to be considered
    -- a successful parse.
    parse :: [String] -> (Maybe a, Int)


simpleParse :: (String -> Maybe a) -> [String] -> (Maybe a, Int)
simpleParse parser = \case
        [] -> (Nothing, 0)
        s : _ -> case parser s of
            Nothing -> (Nothing, 0)
            Just x -> (Just x, 1)


instance Parseable Int where
    parse = simpleParse readMaybe


instance Parseable String where
    parse = simpleParse Just


instance Parseable Float where
    parse = simpleParse readMaybe


instance (Parseable a) => Parseable (Maybe a) where
    parse args = case parse args of
        (Nothing, n) -> (Just Nothing, n)
        (Just x, n) -> (Just $ Just x, n)


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


instance Parseable HelpDescription where
    parse _ = (Just $ HelpDescription "", 0)


--------------------------------------------------------------------------------


data Opaque :: * where
    Opaque :: (Typeable a) => a -> Opaque


type OpaqueCallback m = [Opaque] -> m ()


--------------------------------------------------------------------------------


type OpaqueParser = [String] -> (Maybe Opaque, Int)


parseOpaque :: forall a. (Parseable a, Typeable a) => Proxy a -> OpaqueParser
parseOpaque ~Proxy str = case parse str of
    (Nothing, n) -> (Nothing, n)
    (Just (x :: a), n) -> (Just $ Opaque x, n)


--------------------------------------------------------------------------------


class GetOpaqueParsers f where
    getOpaqueParsers :: Proxy f -> [(TypeRep, OpaqueParser)]


instance (Parseable a, Typeable a, GetOpaqueParsers b) => GetOpaqueParsers (a -> b) where
    getOpaqueParsers ~Proxy = let
        proxyA = Proxy :: Proxy a
        proxyB = Proxy :: Proxy b
        typeRep = typeOf proxyA
        parser = parseOpaque proxyA
        in (typeRep, parser) : getOpaqueParsers proxyB


instance (Monad m) => GetOpaqueParsers (m ()) where
    getOpaqueParsers ~Proxy = []


--------------------------------------------------------------------------------


class WrapCallback m f where
    wrap :: f -> OpaqueCallback m


instance WrapCallback m (m ()) where
    wrap action = \case
        [] -> action
        _ -> internalError


instance (Typeable a, WrapCallback m b) => WrapCallback m (a -> b) where
    wrap f = \case
        Opaque o : os -> case cast o of
            Just x -> let
                g = f x
                g' = wrap g
                in g' os
            Nothing -> internalError
        [] -> internalError


--------------------------------------------------------------------------------


-- | Describes the callback 'f' to be called for a successfully parsed option.
--
-- The function (or value) 'f' can have any arity and ultimately returns a value with type @Monad m => m ()@
--
-- Each of the callback's arguments must have a type 't' which implements 'Parseable' and 'Data.Typeable.Typeable'.
--
-- Think of this as the following constraint synonym:
--
-- > type OptionCallback m f = (Monad m, f ~ (Parseable t*, Typeable t*) => t0 -> t1 -> ... -> tN -> m ())
--
-- Example callbacks:
--
-- > f0 = putStrLn "Option parsed!" :: IO ()
-- > f1 = put :: String -> State String ()
-- > f2 n = liftIO (print n) :: (MonadIO m) => Int -> m ()
-- > f3 name year ratio = lift (print (name, year, ratio)) :: (MonadTrans m) => String -> Int -> Float -> m IO ()
type OptionCallback m f = (Monad m, GetOpaqueParsers f, WrapCallback m f)


-- | An option keyword, such as @"--help"@
data Keyword = Keyword {
    kwNames :: [String], -- ^ All the aliases for this keyword.
    kwArgText :: String, -- ^ Text to describe the arguments to the option given by this keyword.
    kwText :: String -- ^ Text to describe the function of the option given by this keyword.
} deriving (Show, Eq, Ord)


instance IsString Keyword where
    fromString name = kw [name]


class ToKeyword a where
    toKeyword :: a -> Keyword


instance ToKeyword Keyword where
    toKeyword = id


instance ToKeyword String where
    toKeyword name = toKeyword [name]


instance ToKeyword [String] where
    toKeyword names = Keyword {
        kwNames = names,
        kwArgText = "",
        kwText = "" }


-- | Convenience 'Keyword' to build upon.
-- Takes either a single alias or a list of name aliases to start with.
-- Use record syntax to set the rest.
kw :: (ToKeyword a) => a -> Keyword
kw = toKeyword


internalizeKw :: Keyword -> Keyword
internalizeKw k = k {
    kwNames = nub $ sort $ kwNames k }


--------------------------------------------------------------------------------



data OptionInfo m = OptionInfo {
    optionKeyword :: Keyword,
    optionTypeReps :: [TypeRep],
    optionOpaqueCallback :: OpaqueCallback m
} deriving ()


--------------------------------------------------------------------------------


-- | A monad transformer for parsing options.
newtype Options m a = Options {
    unOptions :: StateT (OptionsState m) m a
} deriving (Applicative, Functor, Monad, MonadState (OptionsState m), MonadIO)


instance MonadTrans Options where
    lift = Options . lift


data OptionsState m = OptionsState {
    stateOpaqueParsers :: Map TypeRep OpaqueParser,
    stateOptionsByArity :: [[OptionInfo m]],
    stateCollectedActions :: m (),
    stateCurrMark :: Int,
    stateHighMark :: Int,
    stateArgs :: [String]
} deriving ()


-- | Contains information about what went wrong during an unsuccessful options parse.
data OptionsError
    -- | Contains @(error-message)@ @(begin-args-index)@ @(end-args-index)@
    = ParseFailed String Int Int
    deriving (Show)


mkParseFailed :: Int -> Int -> [String] -> OptionsError
mkParseFailed beginIndex endIndex args = ParseFailed (mkParseFailed' beginIndex endIndex args) beginIndex endIndex


mkParseFailed' :: Int -> Int -> [String] -> String
mkParseFailed' beginIndex endIndex args
    | endIndex == beginIndex + 1 = "Unknown option at index " ++ beginIndexStr ++ ": `" ++ begin ++ "'"
    | endIndex == length args + 1 = "Bad input for `" ++ begin ++ "' at index " ++ beginIndexStr ++ ": End of input."
    | otherwise = "Bad input for `" ++ begin ++ "' at index " ++ beginIndexStr ++ ": `" ++ end ++ "'"
    where
        begin = args !! beginIndex
        end = args !! (endIndex - 1)
        beginIndexStr = show beginIndex


-- | Tries to parse the supplied options against input arguments.
-- If successful, parsed option callbacks are executed. Otherwise
-- __/none/__ of the callbacks are executed.
--
-- Example:
--
-- @
-- import System.Environment
-- import Text.LambdaOptions
-- 
-- options :: Options IO ()
-- options = do
--     addOption (kw "--help") $ do
--         putStrLn "--user NAME [AGE]"
--     addOption (kw "--user") $ \name -> do
--         putStrLn $ "Name:" ++ name
--     addOption (kw "--user") $ \name age -> do
--         putStrLn $ "Name:" ++ name ++ " Age:" ++ show (age :: Int)
-- 
-- main :: IO ()
-- main = do
--     args <- getArgs
--     mError <- runOptions options args
--     case mError of
--         Just (ParseFailed msg _ _) -> putStrLn msg
--         Nothing -> return ()
-- @
runOptions :: (Monad m) => Options m a -> [String] -> m (Maybe OptionsError)
runOptions action args = runOptions' args $ runStateT (unOptions $ action >> tryParseAll) $ OptionsState {
    stateOpaqueParsers = Map.empty,
    stateOptionsByArity = [],
    stateCollectedActions = return (),
    stateCurrMark = 0,
    stateHighMark = 0,
    stateArgs = args }


runOptions' :: (Monad m) => [String] -> m (Bool, OptionsState m) -> m (Maybe OptionsError)
runOptions' args m = m >>= \case
    (True, st) -> stateCollectedActions st >> return Nothing
    (False, st) -> return $ Just $ let
        currMark = stateCurrMark st
        highMark = stateHighMark st
        in mkParseFailed currMark (highMark + 1) args


addByArity :: a -> [[a]] -> Int -> [[a]]
addByArity x xss = \case
    0 -> case xss of
        [] -> [[x]]
        xs : rest -> (x : xs) : rest
    n -> case xss of
        [] -> [] : addByArity x [] (n - 1)
        xs : rest -> xs : addByArity x rest (n - 1)


-- | Adds the supplied option to the @Options m ()@ context.
--
-- If the keyword is matched and the types of the callback's parameters can successfully be parsed, the
-- callback is called with the parsed arguments.
addOption :: (OptionCallback m f) => Keyword -> f -> Options m ()
addOption (internalizeKw -> kwd) f = do
    let (typeReps, opaqueParsers) = unzip $ getOpaqueParsers $ mkProxy f
        arity = length typeReps
        f' = wrap f
        info = OptionInfo {
            optionKeyword = kwd,
            optionTypeReps = typeReps,
            optionOpaqueCallback = f' }
    forM_ (zip typeReps opaqueParsers) $ \(typeRep, opaqueParser) -> do
        modify $ \st -> st { stateOpaqueParsers = Map.insert typeRep opaqueParser $ stateOpaqueParsers st }
    modify $ \st -> st { stateOptionsByArity = addByArity info (stateOptionsByArity st) arity }


firstM' :: (Monad m) => [m Bool] -> m Bool
firstM' = liftM isJust . firstM id


tryParseAll :: (Monad m) => Options m Bool
tryParseAll = do
    whileM_ tryParse $ return ()
    gets (null . stateArgs)


tryParse :: (Monad m) => Options m Bool
tryParse = gets (null . stateArgs) >>= \case
    True -> return False
    False -> tryParseByArity


tryParseByArity :: (Monad m) => Options m Bool
tryParseByArity = do
    optionsByArity <- gets $ reverse . stateOptionsByArity
    firstM' $ map tryParseByOptions optionsByArity


tryParseByOptions :: (Monad m) => [OptionInfo m] -> Options m Bool
tryParseByOptions = firstM' . map tryParseByOption


tryParseByOption :: (Monad m) => OptionInfo m -> Options m Bool
tryParseByOption option = do
    restorePoint <- get
    matchKeyword (optionKeyword option) >>= \case
        False -> return False
        True -> do
            let knownParsers = stateOpaqueParsers restorePoint
            args <- gets stateArgs
            beginMark <- gets stateCurrMark
            let typeReps = optionTypeReps option
                opaqueParsers = mapMaybe (flip Map.lookup knownParsers) typeReps
                (mOpaques, n) = sequenceParsers args opaqueParsers
                args' = drop n args
            result <- case mOpaques of
                Nothing -> do
                    put restorePoint
                    return False
                Just opaques -> do
                    opaques' <- mapM handleSpecialOpaque opaques
                    let action = optionOpaqueCallback option opaques'
                    modify $ \st -> st {
                        stateCurrMark = beginMark + n,
                        stateCollectedActions = stateCollectedActions st >> action,
                        stateArgs = args' }
                    return True
            modify $ \st -> let
                oldHighMark = stateHighMark st
                newHighMark = max oldHighMark (beginMark + n)
                in st { stateHighMark = newHighMark }
            return result


handleSpecialOpaque :: (Monad m) => Opaque -> Options m Opaque
handleSpecialOpaque opaque@(Opaque o) = case cast o of
    Just (HelpDescription _) -> do
        desc <- createHelpDescription
        return $ Opaque $ HelpDescription desc
    _ -> return opaque


matchKeyword :: (Monad m) => Keyword -> Options m Bool
matchKeyword kwd = gets stateArgs >>= \case
    [] -> return False
    (arg : rest) -> case matchKeyword' arg kwd of
        Nothing -> return False
        Just n -> do
            modify $ \st -> let
                newCurrMark = stateCurrMark st + n
                in st {
                    stateCurrMark = newCurrMark,
                    stateHighMark = max newCurrMark (stateHighMark st),
                    stateArgs = rest }
            return True


matchKeyword' :: String -> Keyword -> Maybe Int
matchKeyword' arg kwd = case kwNames kwd of
    [] -> Just 0
    names -> case any (arg ==) names of
        False -> Nothing
        True -> Just 1


sequenceParsers :: [String] -> [OpaqueParser] -> (Maybe [Opaque], Int)
sequenceParsers args = \case
    [] -> (Just [], 0)
    p : ps -> case p args of
        (Nothing, n) -> (Nothing, n)
        (Just o, n) -> let
            rest = drop n args
            in case sequenceParsers rest ps of
                (Nothing, n') -> (Nothing, n + n')
                (Just os, n') -> (Just $ o : os, n + n')


collectKeywords :: (Monad m) => Options m [Keyword]
collectKeywords = gets $ sortBy cmp . map optionKeyword . concat . stateOptionsByArity
    where
        cmp = namesCmp `on` kwNames
        namesCmp [] [] = EQ
        namesCmp [] _ = LT
        namesCmp _ [] = GT
        namesCmp ns1 ns2 = (compare `on` head) ns1 ns2


createHelpDescription :: (Monad m) => Options m String
createHelpDescription = liftM runFormatter collectKeywords


--------------------------------------------------------------------------------


data FormattingConfig = FormattingConfig {
    fmtMaxWidth :: Int
} deriving (Show, Read, Eq, Ord)


data FormatterState = FormatterState {
    fmtConfig :: FormattingConfig,
    fmtEmittedChars :: [Char],
    fmtWord :: [Char],
    fmtWidth :: Int,
    fmtIndentation :: Int
} deriving ()


type Formatter = State FormatterState


runFormatter :: [Keyword] -> String
runFormatter = reverse . fmtEmittedChars . flip execState st . mapM_ formatKeyword
    where
        st = FormatterState {
            fmtConfig = FormattingConfig {
                fmtMaxWidth = 80 },
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


formatKeywordNames :: Keyword -> Formatter ()
formatKeywordNames kwd = do
    let names = sortBy cmp $ kwNames kwd
        (mShortName, otherNames) = case names of
            name : rest -> case isShort name of
                True -> (Just name, rest)
                False -> (Nothing, names)
            [] -> (Nothing, [])
        shortIdx = 0 :: Int
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


formatKeywordArgText :: Keyword -> Formatter ()
formatKeywordArgText kwd = case kwArgText kwd of
    "" -> return ()
    argText -> do
        _ <- flushWord
        changeIndentation . succ =<< gets fmtWidth
        emitString argText


formatKeywordText :: Keyword -> Formatter()
formatKeywordText kwd = do
    _ <- flushWord
    case kwText kwd of
        "" -> return ()
        text -> do
            changeIndentation . succ =<< gets fmtWidth
            changeIndentation 29
            emitString text


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
    c -> modify $ \st -> st {
        fmtWord = c : fmtWord st }


emitString :: String -> Formatter ()
emitString = mapM_ emitChar














