module GitToggl.Options
  where

import Control.Applicative ((<$>), pure)
import Data.Monoid ((<>))
import Options.Applicative hiding (Parser)
import qualified Options.Applicative as Options (Parser)

import GitToggl.Types

parseOptions :: IO Options
parseOptions = execParser optionsParserInfo

optionsParserInfo :: ParserInfo Options
optionsParserInfo = info (helper <*> optionsParser) $
               fullDesc <> progDesc ("Try running `git-toggl init` and " ++
                                     "making a commit")
                        <> header "git-toggl - Toggl Time Tracking for Git"

optionsParser :: Options.Parser Options
optionsParser = subparser ( command "init" (info initC
                  ( progDesc "Sets-up everything and starts a new time entry" ))
           <> command "clean" (info cleanC
                  ( progDesc "Walks you through logging-in to Toggl" ))
           <> command "login" (info loginC
                  ( progDesc "Walks you through logging-in to Toggl" ))
           <> command "prepare-commit-msg" (info prepareC
                  ( progDesc ("If a `current` time entry exists," ++
                              " mentions it on the COMMIT_MSG")))
           <> command "commit-msg" (info commitMsgC
                  ( progDesc ("Caches the last timed commit for later Toggl " ++
                              "API submission.")))
            )
  where
    commitMsgC = CommitMsg <$> argument str ( metavar "COMMIT_FILE" )
    prepareC = PrepareCommitMsg <$> argument str ( metavar "COMMIT_FILE" )
    initC = Init <$> optional (strOption
               ( long "path"
              <> metavar "DIR"
              <> help "Where your git repository is located" ))
    cleanC = Clean <$> optional (strOption
               ( long "path"
              <> metavar "DIR"
              <> help "Where your git repository is located"))
    loginC = pure Login
