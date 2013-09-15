{-# language PackageImports #-}

module Edith.Actions where


import Control.Applicative
import Control.Monad.Trans.Class
import "mtl" Control.Monad.State
import UI.NCurses
import Data.Monoid
import Data.Accessor ((%:), (%=), (.>))
import Control.Arrow

import Edith.Core
import Edith.Buffer as Buffer


exit :: Edith ()
exit = exitFlag %= True

saveFile :: Edith ()
saveFile = do
    e <- get
    liftIO $ writeFile (filePath e) (unlines $ init $ contents_ $ buffer_ e)
    status "saved"

arrowKeysCursorMovements :: Handler
arrowKeysCursorMovements = mconcat $
    EventSpecialKey KeyUpArrow    =: (buffer .> cursorPosition %: first  pred) :
    EventSpecialKey KeyDownArrow  =: (buffer .> cursorPosition %: first  succ) :
    EventSpecialKey KeyLeftArrow  =: (buffer .> cursorPosition %: second pred) :
    EventSpecialKey KeyRightArrow =: (buffer .> cursorPosition %: second succ) :
    []

homeEndMovements :: Handler -> Handler
homeEndMovements parent =
    '\ESC' ==%: (,) (return ()) (Just $ mconcat $
        'O' ==%: (,) (return ()) (Just $ mconcat $
            'F' ==%: (buffer %: endMovement, Just parent) :
            'H' ==%: (buffer %: homeMovement, Just parent) :
            toParent :
            []
        ) :
        toParent :
        []
    )
  where
    toParent = Handler $ const $ Just (status "back", Just parent)

backspaceHandler :: Handler
backspaceHandler =
    EventSpecialKey KeyBackspace =: (buffer %: backspace)

insertCharacter :: Handler
insertCharacter = Handler $ \ event ->
    case event of
        EventCharacter c -> Just (buffer %: Buffer.insertCharacter c, Nothing)
        _ -> Nothing
