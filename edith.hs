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
import Edith.BuildScripts (buildScriptHandler)


main :: IO ()
main = do
    [file] <- getArgs
    buffer <- Edith.Buffer.bufferFromFile file
    deepseq buffer (return ())
    let state = EState False 4 [] file buffer myHandler
    runEdith state


myHandler :: Handler
myHandler = mconcat $
    '\DC3' ==: saveFile : -- Ctrl + S
    '\DC1' ==: exit :     -- Ctrl + Q
    arrowKeysCursorMovements :
    homeEndMovements myHandler :
    backspaceHandler :
    deleteHandler :

    logResizer :

    buildScriptHandler :

    insertCharacter :
    []

