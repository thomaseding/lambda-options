{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Text.LambdaOptions (
    Options,
    Keyword,
    OptionCallback,
    addOption,

    OptionsError(..),
    runOptions,

    Parseable(..),
    List(..),
) where


import Control.Applicative
import Control.Monad.Loops
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Proxy
import Data.Typeable
import Text.Read (readMaybe)


--------------------------------------------------------------------------------


internalError :: a
internalError = error "Internal logic error."


--------------------------------------------------------------------------------


-- | A simple wrapper over @[a]@. Used to avoid overlapping instances for @Parseable [a]@ and @Parseable String@
newtype List a = List [a]
    deriving (Show, Read, Eq, Ord)


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


-- | The callback to be called for a successfully parsed option.
--
-- This function (or value) can have any arity and ultimately returns a value with type @Monad m => m ()@
--
-- Each of the callback's arguments must have a type 't' which implements 'Parseable' and 'Data.Typeable.Typeable'.
--
-- Think of this as the following constraint synonym:
--
-- > type OptionCallback m f = (Monad m, f ~ (Parseable t*, Typeable t*) => t0 -> t1 -> ... -> tN -> m ())
--
-- Example callbacks:
--
-- > putStrLn "Option parsed!" :: IO ()
-- > put :: String -> State String ()
-- > \n -> liftIO (print n) :: (MonadIO m) => Int -> m ()
-- > \name year ratio -> lift (print (name, year, ratio)) :: (MonadTrans m) => String -> Int -> Float -> m IO ()
type OptionCallback m f = (Monad m, GetOpaqueParsers f, WrapCallback m f)


-- | An option keyword, such as @"--help"@
--
-- NB: In the future, this will become a proper data type that contains a list of aliases and help descriptions.
type Keyword = String


data OptionInfo m = OptionInfo {
    optionKeyword :: Keyword,
    optionTypeReps :: [TypeRep],
    optionCallback :: OpaqueCallback m
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
-- options :: Options IO ()
-- options = do
--     addOption "--help" $ do
--         putStrLn "--user NAME [AGE]"
--     addOption "--user" $ \name -> do
--         putStrLn $ "Name:" ++ name
--     addOption "--user" $ \name age -> do
--         putStrLn $ "Name:" ++ name ++ " Age:" ++ show (age :: Int)
--
-- main :: IO ()
-- main = do
--     args <- getArgs
--     mError <- runOptions options args
--     case mError of
--         Just (ParseFailed _ _ _) -> exitFailure
--         Nothing -> exitSuccess
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
addOption :: forall m f. (OptionCallback m f) => Keyword -> f -> Options m ()
addOption keyword f = do
    let (typeReps, opaqueParsers) = unzip $ getOpaqueParsers (Proxy :: Proxy f)
        arity = length typeReps
        f' = wrap f
        info = OptionInfo {
            optionKeyword = keyword,
            optionTypeReps = typeReps,
            optionCallback = f' }
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
                    let action = optionCallback option opaques
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


matchKeyword :: (Monad m) => Keyword -> Options m Bool
matchKeyword kw = gets stateArgs >>= \case
    [] -> return False
    (arg : rest) -> case kw == arg of
        False -> return False
        True -> do
            modify $ \st -> let
                newCurrMark = stateCurrMark st + 1
                in st {
                    stateCurrMark = newCurrMark,
                    stateHighMark = max newCurrMark (stateHighMark st),
                    stateArgs = rest }
            return True


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



