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
import qualified Edith.Buffer


main :: IO ()
main = do
    [file] <- getArgs
    buffer <- Edith.Buffer.Buffer (0, 0) <$> lines <$> readFile file
    deepseq buffer (return ())
    let state = EState file buffer myHandler
    runEdith state


myHandler :: Handler
myHandler = mconcat $
    (EventSpecialKey KeyInsertCharacter) =%:
        (status "command-mode", Just commandHandler) :
    arrowKeysCursorMovement :
    backspaceHandler :
    insertCharacter :
    []

commandHandler :: Handler
commandHandler = mconcat $
    's' ==%: (saveFile, Just myHandler) :
    []



