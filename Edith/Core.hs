{-# language PackageImports, TemplateHaskell #-}

module Edith.Core where


import Control.Applicative
import Data.Accessor.Template
import Data.Monoid
import "mtl" Control.Monad.State
import UI.NCurses


type Edith a = StateT EState Curses a

data EState = EState {
    cursorPosition_ :: (Integer, Integer),
    filePath :: FilePath,
    buffer :: String,

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
    waitFor w

waitFor :: Window -> Edith ()
waitFor w = loop
  where
    loop = do
        updateGUI
        Just event <- lift $ getEvent w Nothing
        liftIO $ appendFile "events" (show event ++ "\n")
        state <- get
        case (getHandler (handler state)) event of
            Nothing -> do
                status ("unknown event: " ++ show event)
                loop
            Just (action, mNewHandler) -> do
                action
                case mNewHandler of
                    Just newHandler ->
                        modify (\ s -> s{handler = newHandler})
                    Nothing -> return ()
                loop

updateGUI :: Edith ()
updateGUI = do
    b <- buffer <$> get
    lift $ do
        def <- defaultWindow
        (height, width) <- screenSize
        updateWindow def $ do
            forM_ (zip (lines b) [0 .. (height - 3)]) $ \ (line, n) -> do
                moveCursor n 0
                drawString (line ++ replicate (fromIntegral width) ' ')
            moveCursor (height - 2) 0
            drawLineH Nothing width -- glyphLineH
    resetCursor
    lift $ render

resetCursor :: Edith ()
resetCursor = do
    position <- cursorPosition_ <$> get
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
