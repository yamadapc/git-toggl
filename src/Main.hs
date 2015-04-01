{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
import Data.Hourglass
import Data.Int
import Options.Applicative
import System.Directory
import System.Exit
import System.FilePath
import System.Hourglass
import System.IO
import System.Process

data TogglAuth = Token String

data Options = Init (Maybe FilePath)
             | PrepareCommitMsg FilePath
             | Login

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    execParser opts >>= run
  where
    run (PrepareCommitMsg cpth) = prepareCommit cpth
    run (Init Nothing) = gitRepositoryPth >>= \case
        Nothing -> do
            hPutStrLn stderr "Not in a git repository"
            exitWith (ExitFailure 1)
        Just pth -> run (Init (Just pth))
    run (Init (Just pth)) = writeCurrent pth
    run Login = loginToggl
    opts = info (helper <*> togglOpts) $
               fullDesc <> progDesc "usage: git-toggl <command> [<args>]"
                        <> header "git-toggl - Toggl Time Tracking for Git"

togglOpts :: Parser Options
togglOpts = subparser ( command "init" (info initC
                  ( progDesc "" ))
           <> command "login" (info loginC
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
    loginC = pure Login

loginToggl :: IO ()
loginToggl = do
    putStr "Looking for cached token... "
    mcached <- findCachedAuth
    case mcached of
        Just (_, fp) -> putStrLn $ "(cached token found at " ++ fp ++ ")"
        Nothing -> do
            putStrLn "(not found)"
            putStrLn "Your token is available on this link:"
            putStrLn "    https://toggl.com/app/profile"
            execOpen "https://toggl.com/app/profile"
            putStr "token: "
            token <- getLine
            hom <- getHomeDirectory
            createDirectoryIfMissing True (hom </> ".toggl")
            writeFile (hom </> ".toggl" </> "token") token
            putStrLn "Token cached"

execOpen :: String -> IO ()
execOpen t = do
    let cmd = "open " ++ t
    h <- spawnCommand cmd
    e <- waitForProcess h
    case e of
        ExitFailure i ->
            hPutStrLn stderr $
                "Command `" ++ cmd ++ "` failed with: " ++ show i
        ExitSuccess -> return ()

findCachedAuth :: IO (Maybe (TogglAuth, FilePath))
findCachedAuth = do
    hom <- (</> ".toggl") <$> getHomeDirectory
    mgit <- gitRepositoryPth
    ma <- case mgit of
        Nothing -> findCacheFileTracked hom
        Just git -> do
            ma <- findCacheFileTracked $ git </> "toggl"
            case ma of
                Nothing -> findCacheFileTracked hom
                j -> return j
    case ma of
        Nothing -> return Nothing
        Just (tok, fp) -> return $ Just (Token tok, fp)
  where
    findCacheFileTracked p = findCacheFile p >>= \case
        Just f -> return $ Just (f, p)
        Nothing -> return Nothing

findCacheFile :: FilePath -> IO (Maybe String)
findCacheFile start = doesFileExist tok >>= \exists ->
    if exists
        then readFile tok >>= \c -> return $ Just c
        else return Nothing
  where
    tok = start </> "token"

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
