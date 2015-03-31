{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
import Data.Hourglass
import Data.Int
import Options.Applicative
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Hourglass
import System.IO

data GitToggl = GitToggl { gitTogglBasePth :: String
                         , gitTogglAuth :: TogglAuth
                         }

data TogglAuth = Token String
               | Cookie String

data Options = Init (Maybe FilePath)
             | PrepareCommitMsg FilePath


togglOpts :: Parser Options
togglOpts = subparser ( command "init" (info initC
                  ( progDesc "" ))
           <> command "prepare-commit-msg" (info prepareC
                  ( progDesc "" ))
            )
  where
    prepareC = PrepareCommitMsg <$> argument str ( metavar "COMMIT_FILE" )
    initC = Init <$> optional (strOption
               ( long "path"
              <> metavar "FILE"
              <> help "Where your git repository is located" ))

main :: IO ()
main = execParser opts >>= run
  where
    run (PrepareCommitMsg cpth) = prepareCommit cpth
    run (Init Nothing) = gitRepositoryPth >>= \case
        Nothing -> do
            hPutStrLn stderr "Not in a git repository"
            exitWith (ExitFailure 1)
        Just pth -> run (Init (Just pth))
    run (Init (Just pth)) = writeCurrent pth
    opts = info (helper <*> togglOpts) $
               fullDesc <> progDesc "usage: git-toggl <command> [<args>]"
                        <> header "git-toggl - Toggl Time Tracking for Git"

prepareCommit :: FilePath -> IO ()
prepareCommit cpth = mstartIO >>= \case
    Nothing -> do
        hPutStrLn stderr ("Couldn't parse " ++ target)
        exitWith (ExitFailure 1)
    Just start -> do
        stop <- timeCurrent
        let (duration, _) = fromSeconds (timeDiff stop start)
            sduration = printDuration duration
        appendFile cpth sduration
  where
    dir = takeDirectory cpth </> "toggl"
    target = dir </> "current"
    mstartIO = timeParse ISO8601_DateAndTime <$> readFile target

printDuration :: Duration -> String
printDuration Duration{..} | (Hours hs)   <- durationHours
                           , (Minutes ms) <- durationMinutes
                           , (Seconds s)  <- durationSeconds = f hs ms s
  where
    unit :: Int64 -> String -> String
    unit 0 _ = ""
    unit 1 u = "1 " ++ u
    unit n u = show n ++ " " ++ u ++ "s"
    seconds s = case unit s "second" of
        "" -> ""
        m -> "and " ++ m
    f hs ms s = unwords [unit hs "hour", unit ms "minute", seconds s,
                    "spent on making this commit."]

writeCurrent :: FilePath -> IO ()
writeCurrent pth = do
    t <- timePrint ISO8601_DateAndTime <$> timeCurrent
    putStrLn $ "Writting timestamp to `" ++ target ++ "` (" ++ t ++ ")"
    createDirectoryIfMissing True dir
    writeFile target t
  where
    dir = pth </> "toggl"
    target = dir </> "current"

gitRepositoryPth :: IO (Maybe FilePath)
gitRepositoryPth = getCurrentDirectory >>= findGit
  where
    findGit base = do
        let candidate = base </> ".git"
        inGitRepo <- doesDirectoryExist candidate
        if inGitRepo
           then return (Just candidate)
           else let n = takeDirectory base
                in if n == base
                      then return Nothing
                      else findGit n
