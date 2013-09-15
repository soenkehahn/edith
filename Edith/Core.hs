{-# language PackageImports, TemplateHaskell #-}

module Edith.Core where


import Control.Applicative
import Data.Accessor
import Data.Accessor.Template
import Data.Char
import Data.Monoid
import "mtl" Control.Monad.State
import UI.NCurses

import Edith.Buffer


type Edith a = StateT EState Curses a

data EState = EState {
    exitFlag_ :: Bool,

    filePath :: FilePath,
    buffer_ :: Buffer,

    handler :: Handler
  }

data Handler = Handler {
    getHandler :: (Event -> Maybe (Edith (), Maybe Handler))
  }

instance Monoid Handler where
    mempty = Handler $ const Nothing
    mappend (Handler a) (Handler b) = Handler $ \ event ->
        case a event of
            Just (action, ma') -> Just (action, fmap (<> Handler b) ma')
            Nothing -> case b event of
                Just (action, mb') -> Just (action, fmap (Handler a <>) mb')
                Nothing -> Nothing

$( deriveAccessors ''EState )

runEdith :: EState -> IO ()
runEdith state = runCurses $ flip evalStateT state $ do
    lift $ setEcho False
    w <- lift $ defaultWindow
    mainLoop w

mainLoop :: Window -> Edith ()
mainLoop w = loop
  where
    loop = do
        updateGUI
        Just event <- lift $ getEvent w Nothing
        liftIO $ appendFile "events" (show event ++ "\n")
        state <- get
        case (getHandler (handler state)) event of
            Nothing -> do
                status ("unknown event: " ++ show (show event))
                loop
            Just (action, mNewHandler) -> do
                action
                case mNewHandler of
                    Just newHandler ->
                        modify (\ s -> s{handler = newHandler})
                    Nothing -> return ()
                shouldExit <- exitFlag_ <$> get
                when (not shouldExit) loop

updateGUI :: Edith ()
updateGUI = do
    buffer %: sanitizeCursorPosition
    (height, width) <- lift screenSize
    let bufferHeight = height - 2
    buffer %: sanitizeScrolling bufferHeight
    dBuffer <- displayBuffer bufferHeight . buffer_ <$> get
    lift $ do
        def <- defaultWindow
        updateWindow def $ do
            forM_ (zip dBuffer [0 ..]) $ \ ((lineNumber, line), cursorLine) -> do
                moveCursor cursorLine 0
                drawString (map sanitizeChar line ++ replicate (fromIntegral width) ' ')
            moveCursor (height - 2) 0
            drawLineH Nothing width -- glyphLineH
    status =<< (show . scrolling_ . buffer_) <$> get
    resetCursor bufferHeight
    lift $ render
  where
    sanitizeChar :: Char -> Char
    sanitizeChar c | isPrint c = c
    sanitizeChar _ = '�'

-- ~ resetCursor :: Edith ()
resetCursor bufferHeight = do
    position <- scrolledCursorPosition . buffer_ <$> get
    when (fst position > bufferHeight) $
        error (show position)
    lift $ do
        def <- defaultWindow
        updateWindow def $ do
            uncurry moveCursor position


status :: String -> Edith ()
status msg = do
    lift $ do
        (height, _) <- screenSize
        def <- defaultWindow
        updateWindow def $ do
            moveCursor (height - 1) 0
            drawString (msg ++ replicate (fromIntegral height) ' ')


-- * convenience

(=%:) :: Event -> (Edith (), Maybe Handler) -> Handler
e =%: action = Handler $ \ event ->
    if event == e then Just action else Nothing

(==%:) :: Char -> (Edith (), Maybe Handler) -> Handler
c ==%: action = EventCharacter c =%: action

(=:) :: Event -> Edith () -> Handler
event =: action = event =%: (action, Nothing)
