{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}


module LambdaOptions (
    Parseable(..),
    Keyword,
    OptionCallback,
    Options,
    OptionsError(..),
    addOption,
    runOptions,
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


-- | Class describing parseable values. Much like the 'Prelude.Read' class.
class Parseable a where
    -- | Given a 'String', returns @Just a@ if and only if the __/entire/__ string can be parsed.
    parse :: String -> Maybe a


instance Parseable Int where
    parse = readMaybe


instance Parseable String where
    parse = Just


instance Parseable Float where
    parse = readMaybe


--------------------------------------------------------------------------------


data Opaque :: * where
    Opaque :: (Typeable a) => a -> Opaque


type OpaqueCallback m = [Opaque] -> m ()


--------------------------------------------------------------------------------


type OpaqueParser = String -> Maybe Opaque


parseOpaque :: forall a. (Parseable a, Typeable a) => Proxy a -> OpaqueParser
parseOpaque ~Proxy str = case parse str of
    Nothing -> Nothing
    Just (x :: a) -> Just $ Opaque x


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
-- Each of the callback's arguments must have a type 't' which implements 'Parseable'.
--
-- Example callbacks:
--
-- > putStrLn "Option parsed!" :: IO ()
-- > put :: String -> State ()
-- > \n -> liftIO (print n) :: (MonadIO m) => Int -> m ()
-- > \n s f -> lift (print (n, s, f)) :: (MonadTrans m) => Int -> String -> Float -> m IO ()
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
    stateArgs :: [String]
} deriving ()


-- | Contains information about what went wrong during an unsuccessful parse.
data OptionsError
    -- | NB: In the future, there will be more informative constructors.
    = OptionsError
    deriving (Show)


-- | Tries to parse the supplied options against input arguments.
--   If successful, parsed option callbacks are executed.
runOptions :: (Monad m) => Options m a -> [String] -> m (Maybe OptionsError)
runOptions action args = runOptions' $ runStateT (unOptions $ action >> tryParseAll) $ OptionsState {
    stateOpaqueParsers = Map.empty,
    stateOptionsByArity = [],
    stateCollectedActions = return (),
    stateArgs = args }


runOptions' :: (Monad m) => m (Bool, OptionsState m) -> m (Maybe OptionsError)
runOptions' m = m >>= \case
    (True, st) -> stateCollectedActions st >> return Nothing
    (False, _) -> return $ Just OptionsError


addByArity :: a -> [[a]] -> Int -> [[a]]
addByArity x xss = \case
    0 -> case xss of
        [] -> [[x]]
        xs : rest -> (x : xs) : rest
    n -> case xss of
        [] -> [] : addByArity x [] (n - 1)
        xs : rest -> xs : addByArity x rest (n - 1)


-- | Adds the following option into the monadic context.
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
            let typeReps = optionTypeReps option
                arity = length typeReps
                opaqueParsers = mapMaybe (flip Map.lookup knownParsers) typeReps
                opaques = catMaybes $ zipWith ($) opaqueParsers args
            case length opaques == arity of
                False -> do
                    put restorePoint
                    return False
                True -> do
                    let action = optionCallback option opaques
                    modify $ \st -> st {
                        stateCollectedActions = stateCollectedActions st >> action,
                        stateArgs = drop arity $ stateArgs st }
                    return True


matchKeyword :: (Monad m) => Keyword -> Options m Bool
matchKeyword kw = gets stateArgs >>= \case
    [] -> return False
    (arg : rest) -> case kw == arg of
        False -> return False
        True -> do
            modify $ \st -> st { stateArgs = rest }
            return True



