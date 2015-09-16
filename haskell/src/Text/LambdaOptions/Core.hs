{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Text.LambdaOptions.Core (
    runOptions,
    Options,
    OptionsError(..),

    OptionCallback,
    addOption,

    getHelpDescription,

    getKeywords
) where


import Control.Applicative
import Control.Monad.State
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Proxy
import Data.Typeable hiding (typeRep)
import Text.LambdaOptions.Formatter
import Text.LambdaOptions.Internal.Opaque
import Text.LambdaOptions.Internal.OpaqueParser
import Text.LambdaOptions.Internal.Wrap
import Text.LambdaOptions.Keyword
import Text.LambdaOptions.Parseable


--------------------------------------------------------------------------------


getProxy :: a -> Proxy a
getProxy _ = Proxy


--------------------------------------------------------------------------------


-- | Describes the callback 'f' to be called for a successfully parsed option.
--
-- The function (or value) 'f' can have any arity and ultimately returns a value with type @Monad m => m a@
--
-- Each of the callback's arguments must have a type 't' which implements 'Parseable' and 'Data.Typeable.Typeable'.
--
-- Think of this as the following constraint synonym:
--
-- > type OptionCallback m a f = (Monad m, f ~ (Parseable t*, Typeable t*) => t0 -> t1 -> ... -> tN -> m a)
--
-- Example callbacks:
--
-- > f0 = putStrLn "Option parsed!" :: IO ()
-- > f1 = put :: String -> State String ()
-- > f2 n = liftIO (print n) :: (MonadIO m) => Int -> m ()
-- > f3 name year ratio = lift (print (name, year, ratio)) :: (MonadTrans m) => String -> Int -> Float -> m IO ()
-- > f4 = return 7 :: Identity Int
type OptionCallback m a f = (Monad m, GetOpaqueParsers a f, Wrap (m a) f)


internalizeKeyword :: Keyword -> Keyword
internalizeKeyword k = k {
    kwNames = nub $ sort $ kwNames k }


--------------------------------------------------------------------------------


data OptionInfo m a = OptionInfo {
    optionKeyword :: Keyword,
    optionTypeReps :: [TypeRep],
    optionOpaqueCallback :: OpaqueCallback (m a)
} deriving ()


--------------------------------------------------------------------------------


-- | A monad for parsing options.
newtype Options m a b = Options {
    unOptions :: State (OptionsState m a) b
} deriving (Applicative, Functor, Monad, MonadState (OptionsState m a))


data OptionsState m a = OptionsState {
    stateOpaqueParsers :: Map TypeRep OpaqueParser,
    stateOptionsByArity :: [[OptionInfo m a]],
    stateCollectedActions :: [m a],
    stateCurrMark :: Int,
    stateHighMark :: Int,
    stateArgs :: [String],
    stateFormatConfig :: FormatConfig
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
-- If successful, parsed option callbacks are returned in 'Prelude.Right'. Otherwise
-- an 'OptionsError' is returned in 'Prelude.Left'.
--
-- Example program:
--
-- > import System.Environment
-- > import Text.LambdaOptions
-- > 
-- > 
-- > options :: Options IO () ()
-- > options = do
-- >     addOption (kw ["--help", "-h"] `text` "Display this help text.") $ do
-- >         putStrLn "Usage:"
-- >         putStrLn $ getHelpDescription options
-- >     addOption (kw "--user" `argText` "NAME" `text` "Prints name.") $ \name -> do
-- >         putStrLn $ "Name:" ++ name
-- >     addOption (kw "--user" `argText` "NAME AGE" `text` "Prints name and age.") $ \name age -> do
-- >         putStrLn $ "Name:" ++ name ++ " Age:" ++ show (age :: Int)
-- > 
-- > 
-- > main :: IO ()
-- > main = do
-- >     args <- getArgs
-- >     case runOptions options args of
-- >         Left (ParseFailed msg _ _) -> do
-- >             putStrLn msg
-- >             putStrLn $ getHelpDescription options
-- >         Right actions -> sequence_ actions
--
-- >>> example.exe --user HaskellCurry 81 --user GraceHopper
-- Name:HaskellCurry Age:81
-- Name:GraceHopper
-- >>> example.exe -h
-- Usage:
-- -h, --help                  Display this help text.
--     --user NAME             Prints name.
--     --user NAME AGE         Prints name and age.
-- >>> example.exe --user Pythagoras LXXV
-- Unknown option at index 2: `LXXV'
-- Usage:
-- -h, --help                  Display this help text.
--     --user NAME             Prints name.
--     --user NAME AGE         Prints name and age.
runOptions :: (Monad m) => Options m a () -> [String] -> Either OptionsError [m a]
runOptions options args = runOptions' args $ runOptionsInternal defaultFormatConfig args (options >> tryParseAll)


runOptionsInternal :: (Monad m) => FormatConfig -> [String] -> Options m a b -> (b, OptionsState m a)
runOptionsInternal config args options = runState (unOptions options) $ OptionsState {
    stateOpaqueParsers = Map.empty,
    stateOptionsByArity = [],
    stateCollectedActions = [],
    stateCurrMark = 0,
    stateHighMark = 0,
    stateArgs = args,
    stateFormatConfig = config }


runOptions' :: (Monad m) => [String] -> (Bool, OptionsState m a) -> Either OptionsError [m a]
runOptions' args result = case result of
    (True, st) -> Right $ reverse $ stateCollectedActions st
    (False, st) -> Left $ let
        currMark = stateCurrMark st
        highMark = stateHighMark st
        in mkParseFailed currMark (highMark + 1) args


addByArity :: a -> [[a]] -> Int -> [[a]]
addByArity x xss n = case n of
    0 -> case xss of
        [] -> [[x]]
        xs : rest -> (x : xs) : rest
    _ -> case xss of
        [] -> [] : addByArity x [] (n - 1)
        xs : rest -> xs : addByArity x rest (n - 1)


-- | Adds the supplied option to the @Options m a ()@ context.
--
-- If the keyword is matched and the types of the callback's parameters can successfully be parsed, the
-- callback is called with the parsed arguments.
addOption :: (OptionCallback m a f) => Keyword -> f -> Options m a ()
addOption inKwd f = do
    let (typeReps, opaqueParsers) = unzip $ getOpaqueParsers $ getProxy f
        arity = length typeReps
        f' = wrap f
        kwd = internalizeKeyword inKwd
        info = OptionInfo {
            optionKeyword = kwd,
            optionTypeReps = typeReps,
            optionOpaqueCallback = f' }
    forM_ (zip typeReps opaqueParsers) $ \(typeRep, opaqueParser) -> do
        modify $ \st -> st { stateOpaqueParsers = Map.insert typeRep opaqueParser $ stateOpaqueParsers st }
    modify $ \st -> st { stateOptionsByArity = addByArity info (stateOptionsByArity st) arity }


firstM :: (Monad m) => [m Bool] -> m Bool
firstM actions = case actions of
    m : ms -> m >>= \result -> case result of
        False -> firstM ms
        True -> return True
    [] -> return False


whileM :: (Monad m) => m Bool -> m ()
whileM m = m >>= \result -> case result of
    True -> whileM m
    False -> return ()


tryParseAll :: (Monad m) => Options m a Bool
tryParseAll = do
    whileM tryParse
    gets (null . stateArgs)


tryParse :: (Monad m) => Options m a Bool
tryParse = gets (null . stateArgs) >>= \result -> case result of
    True -> return False
    False -> tryParseByArity


tryParseByArity :: (Monad m) => Options m a Bool
tryParseByArity = do
    optionsByArity <- gets $ reverse . stateOptionsByArity
    firstM $ map tryParseByOptions optionsByArity


tryParseByOptions :: (Monad m) => [OptionInfo m a] -> Options m a Bool
tryParseByOptions = firstM . map tryParseByOption


tryParseByOption :: (Monad m) => OptionInfo m a -> Options m a Bool
tryParseByOption option = do
    restorePoint <- get
    matchKeyword (optionKeyword option) >>= \match -> case match of
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
                    let action = optionOpaqueCallback option opaques
                    modify $ \st -> st {
                        stateCurrMark = beginMark + n,
                        stateCollectedActions = action : stateCollectedActions st,
                        stateArgs = args' }
                    return True
            modify $ \st -> let
                oldHighMark = stateHighMark st
                newHighMark = max oldHighMark (beginMark + n)
                in st { stateHighMark = newHighMark }
            return result


matchKeyword :: (Monad m) => Keyword -> Options m a Bool
matchKeyword kwd = gets stateArgs >>= \args -> case args of
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
sequenceParsers args parsers = case parsers of
    [] -> (Just [], 0)
    p : ps -> case p args of
        (Nothing, n) -> (Nothing, n)
        (Just o, n) -> let
            rest = drop n args
            in case sequenceParsers rest ps of
                (Nothing, n') -> (Nothing, n + n')
                (Just os, n') -> (Just $ o : os, n + n')


collectKeywords :: (Monad m) => Options m a [Keyword]
collectKeywords = gets $ sortBy cmp . map optionKeyword . concat . stateOptionsByArity
    where
        cmp = namesCmp `on` kwNames
        namesCmp [] [] = EQ
        namesCmp [] _ = LT
        namesCmp _ [] = GT
        namesCmp ns1 ns2 = (compare `on` head) ns1 ns2


--------------------------------------------------------------------------------


createHelpDescription :: (Monad m) => Options m a String
createHelpDescription = do
    config <- gets stateFormatConfig
    kwds <- collectKeywords
    return $ formatKeywords config kwds


-- | Produces the help description given by the input options.
getHelpDescription :: (Monad m) => Options m a () -> String
getHelpDescription options = fst $ runOptionsInternal defaultFormatConfig [] $ do
    options
    createHelpDescription


--------------------------------------------------------------------------------


-- | Produces the `Keyword`s inserted into the input options.
getKeywords :: (Monad m) => Options m a () -> [Keyword]
getKeywords options = fst $ runOptionsInternal defaultFormatConfig [] $ do
    options
    collectKeywords










