{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
import Control.Monad (unless, when)
import Data.Aeson
import Data.Attoparsec.Text
import qualified Data.ByteString.Lazy as ByteStringL
import qualified Data.ByteString as ByteStringS
import Data.Hourglass
import Data.Int
import Data.List (isInfixOf)
import Data.Text (Text, unpack)
import qualified Data.Text as Text (lines, words)
import Options.Applicative hiding (Parser)
import qualified Options.Applicative as Options (Parser)
import System.Directory
import System.Exit
import System.FilePath
import System.Hourglass
import System.IO
import System.Process (spawnCommand, waitForProcess)
import System.Process.Text (readProcessWithExitCode)

data TogglAuth = Token String

data Options = Init (Maybe FilePath)
             | Clean (Maybe FilePath)
             | PrepareCommitMsg FilePath
             | CommitMsg FilePath
             | Login

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    execParser opts >>= run
  where
    run (PrepareCommitMsg cpth) = prepareCommit cpth
    run (Init Nothing) = gitRepositoryPth >>= \case
        Nothing -> notInAGitRepository
        Just pth -> run (Init (Just pth))
    run (Init (Just pth)) = ensureSetup pth >> writeCurrent pth
    run (CommitMsg pth) = persistCurrent pth
    run (Clean Nothing) = gitRepositoryPth >>= \case
        Nothing -> notInAGitRepository
        Just pth -> run (Clean (Just pth))
    run (Clean (Just pth)) = removeCurrent pth
    run Login = loginToggl
    notInAGitRepository = do
        hPutStrLn stderr "Not in a git repository"
        exitWith (ExitFailure 1)
    opts = info (helper <*> togglOpts) $
               fullDesc <> progDesc ("Try running `git-toggl init` and " ++
                                     "making a commit")
                        <> header "git-toggl - Toggl Time Tracking for Git"

togglOpts :: Options.Parser Options
togglOpts = subparser ( command "init" (info initC
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

loginToggl :: IO ()
loginToggl = do
    putStr "Looking for cached token... "
    mcached <- findCachedAuth
    case mcached of
        Just (_, fp) -> putStrLn $ "(cached token found at " ++ fp ++ ")"
        Nothing -> do
            putStrLn $
                "(not found)\n" ++
                "Your token is available on this link:\n" ++
                "    https://toggl.com/app/profile"
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

ensureSetup :: FilePath -> IO ()
ensureSetup pth = do
    plantGitHook pth "commit-msg"
    plantGitHook pth "prepare-commit-msg"

plantGitHook :: FilePath -> String -> IO ()
plantGitHook pth name = doesFileExist hookPth >>= \exists ->
   if exists
       then do
           putStrLn $ "Hook " ++ hookPth ++ " already exists"
           hook <- readFile hookPth
           unless (("git-toggl " ++ name) `isInfixOf` hook) $
               putStrLn $ "Please add the following to the existing hook:\n" ++
                          "`git-toggl " ++ name ++ "`"
       else do
           putStrLn $ "Writting git-toggl `" ++ name ++ "` hook to " ++ hookPth
           writeFile hookPth ("#!/bin/sh\ngit-toggl " ++ name ++ " $1")
           perms <- getPermissions hookPth
           setPermissions hookPth (setOwnerExecutable True perms)
  where
    hookPth = pth </> "hooks" </> name

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
prepareCommit cpth = doesFileExist target >>= \exists -> do
    mstart <- timeParse ISO8601_DateAndTime <$>
                readFile target

    when exists $ case mstart of
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

writeCurrent :: FilePath -> IO ()
writeCurrent pth = do
    stime <- localTimePrint ISO8601_DateAndTime <$> localDateCurrent
    putStrLn $ "Writting timestamp to `" ++ target ++ "` (" ++ stime ++ ")"
    createDirectoryIfMissing True dir
    writeFile target stime
  where
    dir = pth </> "toggl"
    target = dir </> "current"

persistCurrent :: FilePath -> IO ()
persistCurrent cpth = do
    mstart <- localTimeParse ISO8601_DateAndTime <$> readFile currentPth
    case mstart of
        Nothing -> do
            hPutStrLn stderr ("Couldn't parse " ++ currentPth)
            exitWith (ExitFailure 1)
        Just start -> do
            commit <- getLastCommit currentPth start
            ByteStringL.writeFile (unpack (commitSha commit)) (encode commit)
  where
    currentPth = takeDirectory cpth </> "toggl" </> "current"

data Commit = Commit { commitStart :: LocalTime DateTime
                     , commitStop :: LocalTime DateTime
                     , commitSha :: Text
                     , commitMessage :: Text
                     , commitRepository :: Maybe Repository
                     , commitAuthor :: Text
                     }

instance ToJSON Commit where
    toJSON Commit{..} = o [ "time_entry" .=
                            o [ "start" .= fmt commitStart
                              , "stop" .= fmt commitStop
                              , "description" .= head (Text.lines commitMessage)
                              , "project" .= commitRepository
                              ]
                          , "metadata" .=
                            o [ "commit" .= commitSha
                              , "repository" .= commitRepository
                              ]
                          ]
      where
        fmt = localTimePrint ISO8601_DateAndTime
        o = object

data Repository = GithubRepository String

instance ToJSON Repository where
    toJSON (GithubRepository repo) = toJSON repo

getLastCommit :: FilePath -> LocalTime DateTime -> IO Commit
getLastCommit gitPth start = do
    (code, msg, _) <- readProcessWithExitCode "git" ["log", "-1", "--date=iso8601"] ""
    case code of
        ExitSuccess ->
            case parseOnly commitMsgParser msg of
                Left x -> do
                    print x
                    hPutStrLn stderr "Failed to parse last commit message"
                    exitWith (ExitFailure 1)
                Right (sha, author, stop, message) -> do
                    return Commit { commitRepository = Just (GithubRepository gitPth)
                                  , commitStart = start
                                  , commitStop = stop
                                  , commitSha = sha
                                  , commitMessage = message
                                  , commitAuthor = author
                                  }
        ExitFailure c -> do
            hPutStrLn stderr $
                "Command `git log -1` failed with non-zero exit code - " ++
                show c
            exitWith (ExitFailure 1)

commitMsgParser :: Parser (Text, Text, LocalTime DateTime, Text)
commitMsgParser = do
    _ <- string "commit "
    sha <- takeLine
    endOfLine
    _ <- string "Author: "
    author <- takeLine
    endOfLine
    _ <- string "Date:   "
    sstop <- takeLine
    endOfLine
    message <- consumePadded
    case localTimeParse ISO8601_DateAndTime (fixGitDate (unpack sstop)) of
        Nothing -> fail "Failed to parse"
        Just stop -> return (sha, author, stop, message)
  where
    takeLine = takeTill isEndOfLine
    fixGitDate ds = let [d, t, tz] = words ds
                      in d ++ "T" ++ t ++
                         Prelude.take 3 tz ++ ":" ++ Prelude.drop 3 tz
    consumePadded = do
        skipSpace
        takeLine



removeCurrent :: FilePath -> IO ()
removeCurrent pth = doesFileExist target >>= \exists ->
    if exists
        then do
            putStrLn $ "Removing timestamp at " ++ target
            removeFile target
        else putStrLn "No timestamp to remove"
  where
    target = pth </> "toggl" </> "current"

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
