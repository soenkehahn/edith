{-# language PackageImports #-}

module Edith.Actions where


import Control.Monad.Trans.Class
import "mtl" Control.Monad.State
import UI.NCurses
import Data.Monoid
import Data.Accessor ((%:))
import Control.Arrow

import Edith.Core


saveFile :: Edith ()
saveFile = do
    e <- get
    liftIO $ writeFile (filePath e) (buffer e)
    status "saved"

arrowKeysCursorMovement :: Handler
arrowKeysCursorMovement = mconcat $
    EventSpecialKey KeyUpArrow    =: (cursorPosition %: first  pred) :
    EventSpecialKey KeyDownArrow  =: (cursorPosition %: first  succ) :
    EventSpecialKey KeyLeftArrow  =: (cursorPosition %: second pred) :
    EventSpecialKey KeyRightArrow =: (cursorPosition %: second succ) :
    []
