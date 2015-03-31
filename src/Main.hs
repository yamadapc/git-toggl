import Options.Applicative
import System.Directory
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

togglOpts :: Parser Options
togglOpts = Init <$> optional (strOption
                ( long "path"
               <> metavar "FILE"
               <> help "Where your git repository is located" ))

main :: IO ()
main = execParser opts >>= run
  where
    run (Init Nothing) = gitRepositoryPth >>= \mpth -> case mpth of
        Nothing -> do
            hPutStrLn stderr "Not in a git repository"
            exitWith (ExitFailure 1)
        Just pth -> run (Init (Just pth))
    run (Init (Just pth)) = writeCurrent pth
    opts = info (helper <*> togglOpts) $
               fullDesc <> progDesc "usage: git-toggl <command> [<args>]"
                        <> header "git-toggl - Toggl Time Tracking for Git"

writeCurrent :: FilePath -> IO ()
writeCurrent pth = do
        t <- timeCurrent
        let dir = pth </> "toggl"
            target = dir </> "current"

        putStrLn $ "Writting timestamp to `" ++ target ++ "` (" ++ show t ++ ")"
        createDirectoryIfMissing True dir
        writeFile target (show t)

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
