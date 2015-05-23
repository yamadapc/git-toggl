{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
import Control.Applicative ((<$>))
import Control.Monad (unless, when)
import qualified Data.Aeson as Aeson (encode)
import Data.Attoparsec.Text
import qualified Data.ByteString.Lazy as ByteStringL
import Data.Hourglass
import Data.Int
import Data.List (isInfixOf)
import Data.Text (Text, unpack)
import System.Directory
import System.Exit
import System.FilePath
import System.Hourglass
import System.IO
import System.Process (spawnCommand, waitForProcess)
import System.Process.Text (readProcessWithExitCode)

import GitToggl.Options
import GitToggl.Types

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    parseOptions >>= run
  where
    run (PrepareCommitMsg cpth) = prepareCommit cpth
    run (Init Nothing) = gitRepositoryPth >>= \case
        Nothing -> notInAGitRepository
        Just pth -> run (Init (Just pth))
    run (Init (Just pth)) = ensureSetup pth >> writeCurrent pth
    run (CommitMsg pth) = do
        persistCurrent pth
        run (Clean (Just (takeDirectory pth)))
    run (Clean Nothing) = gitRepositoryPth >>= \case
        Nothing -> notInAGitRepository
        Just pth -> run (Clean (Just pth))
    run (Clean (Just pth)) = removeCurrent pth
    run Login = loginToggl
    notInAGitRepository = do
        hPutStrLn stderr "Not in a git repository"
        exitWith (ExitFailure 1)

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
prepareCommit cpth = doesFileExist target >>= \exists ->
    when exists $ do
        mstart <- timeParse ISO8601_DateAndTime <$> readFile target
        case mstart of
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
persistCurrent cpth = doesFileExist currentPth >>= \exists -> when exists $ do
    mstart <- localTimeParse ISO8601_DateAndTime <$> readFile currentPth
    case mstart of
        Nothing -> do
            hPutStrLn stderr ("Couldn't parse " ++ currentPth)
            exitWith (ExitFailure 1)
        Just start -> do
            commit <- getLastCommit currentPth start
            ByteStringL.writeFile
                (dir </> unpack (commitSha commit))
                (Aeson.encode commit)
  where
    dir = takeDirectory cpth </> "toggl"
    currentPth = dir </> "current"

getLastCommit :: FilePath -> LocalTime DateTime -> IO Commit
getLastCommit gitPth start = do
    (code, msg, _) <- readProcessWithExitCode "git" ["log", "-1", "--date=iso8601"] ""
    case code of
        ExitSuccess ->
            case parseCommitMsg start gitPth msg of
                Left x -> do
                    print x
                    hPutStrLn stderr "Failed to parse last commit message"
                    exitWith (ExitFailure 1)
                Right commit -> return commit
        ExitFailure c -> do
            hPutStrLn stderr $
                "Command `git log -1` failed with non-zero exit code - " ++
                show c
            exitWith (ExitFailure 1)

parseCommitMsg :: LocalTime DateTime -> String -> Text -> Either String Commit
parseCommitMsg start gitPth msg = parseOnly (commitMsgParser start mrepo) msg
  where mrepo = Just (GithubRepository gitPth)

commitMsgParser :: LocalTime DateTime -> Maybe Repository -> Parser Commit
commitMsgParser start mrepo = do
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
        Just stop -> return Commit { commitRepository = mrepo
                                   , commitStart = start
                                   , commitStop = stop
                                   , commitSha = sha
                                   , commitMessage = message
                                   , commitAuthor = author
                                   }
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
