{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | View my source code to see example.
module Text.LambdaOptions.Example.Example_6_SubOptions (
  main,
) where

import           Data.Proxy ( Proxy(Proxy) )
import qualified System.Environment as Env
import qualified Text.LambdaOptions as L

data SourceControlOption
  = Help
  | CloneHelp
  | CloneRepository String String
  | CloneBinary
  deriving (Show)

data TopOption
  = Option SourceControlOption
  | DelegateClone [String]

topOptions :: L.Options TopOption
topOptions = do

  L.addOption
    (L.kw "help"
    `L.text` "Display this help text.")
    $ Option Help

  L.addOption
    (L.kw "help"
    `L.argText` "clone"
    `L.text` "Display help text for the 'clone' options.")
    $ \(Proxy :: Proxy "clone") -> Option CloneHelp

  L.addOption
    (L.kw "clone"
    `L.argText` "ARGS+"
    `L.text` "See 'help clone' for more details.")
    $ \(L.List subArgs) -> DelegateClone subArgs

cloneOptions :: L.Options SourceControlOption
cloneOptions = do

  L.addOption
    (L.kw ()
    `L.argText` "REPO DIR?"
    `L.text` "Clones REPO into DIR.")
    $ \repo mDir -> CloneRepository repo $ maybe "." id mDir

  L.addOption
    (L.kw "--binary"
    `L.text` "Optimize clone for repos with large binary data.")
    $ CloneBinary

parseTopOptions :: [String] -> Either String [SourceControlOption]
parseTopOptions args = case L.runOptions topOptions args of
  Left e -> Left $ unlines
    [ L.prettyOptionsError e
    , L.getHelpDescription topOptions
    ]
  Right options -> case options of
    [option] -> case option of
      DelegateClone args' -> parseCloneOptions args'
      Option opt -> Right [opt]
    _ -> Right [Help]

parseCloneOptions :: [String] -> Either String [SourceControlOption]
parseCloneOptions subArgs = case L.runOptions cloneOptions subArgs of
  Left e -> Left $ unlines
    [ L.prettyOptionsError e
        { L.parseFailedArgs = "clone" : subArgs
        , L.parseFailedBeginArgsIndex = 1 + L.parseFailedBeginArgsIndex e
        , L.parseFailedEndArgsIndex   = 1 + L.parseFailedEndArgsIndex e
        }
    , L.getHelpDescription cloneOptions
    ]
  Right options -> Right $ case options of
    [] -> [CloneHelp]
    _  -> options

-- | Try with these succeeding examples:
--
-- > :main help
-- > :main help clone
-- > :main clone some-repo
-- > :main clone some-repo some-dir
-- > :main clone --binary some-repo
main :: IO ()
main = do
  args <- Env.getArgs
  case parseTopOptions args of
    Left errorMessage -> putStrLn errorMessage
    Right commands -> flip mapM_ commands $ \command ->
      case command of
        Help      -> putStrLn $ L.getHelpDescription topOptions
        CloneHelp -> putStrLn $ L.getHelpDescription cloneOptions
        _         -> putStrLn $ "Executing " ++ show command

