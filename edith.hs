{-# language PackageImports #-}


import UI.NCurses
import Data.Monoid
import Control.Applicative
import Control.Monad.IO.Class
import "mtl" Control.Monad.State
import System.Environment
import Control.DeepSeq

import Edith.Core
import Edith.Actions


main :: IO ()
main = do
    [file] <- getArgs
    buffer <- readFile file
    deepseq buffer (return ())
    let state = EState (0, 0) file buffer myHandler
    runEdith state


myHandler :: Handler
myHandler = mconcat $
    (EventSpecialKey KeyInsertCharacter) =%:
        (status "command-mode", Just commandHandler) :
    arrowKeysCursorMovement :
    []

commandHandler :: Handler
commandHandler = mconcat $
    's' ==%: (saveFile, Just myHandler) :
    []



