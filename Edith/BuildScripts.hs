
module Edith.BuildScripts where


import Control.Monad.IO.Class
import Data.Monoid
import System.Process
import System.Exit
import System.FilePath
import System.Directory
import Data.Foldable

import Edith.Core
import Edith.Actions


buildScriptHandler :: Handler
buildScriptHandler = mconcat $
    ('\STX' ==: runCurrentBuildScript) :
    []

runCurrentBuildScript :: Edith ()
runCurrentBuildScript = do
    saveFile
    cwd <- liftIO $ getCurrentDirectory
    (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode (cwd </> "build.sh") [] ""
    liftIO $ do
        appendFile "build.log" stdout
        appendFile "build.log" stderr
    status ("running build.sh...")
    updateGUI
    forM_ (lines stdout ++ lines stderr) $ status
    case exitCode of
        ExitFailure ec -> do
            status ("error: " ++ show ec)
        ExitSuccess ->
            status "success"
