{-# language TemplateHaskell #-}

module Edith.Buffer where


import Control.Monad.IO.Class
import Control.Applicative
import Data.List
import Data.Monoid
import Data.Accessor
import Data.Accessor.Template
import Control.DeepSeq


data Buffer = Buffer {
    cursorPosition_ :: (Integer, Integer),
    contents_ :: [String]
  }

instance NFData Buffer where
    rnf (Buffer a b) = rnf a `seq` rnf b `seq` ()

$( deriveAccessors ''Buffer )

sanitizeCursorPosition :: Buffer -> Buffer
sanitizeCursorPosition buffer =
    cursorPosition ^: (
        (firstA ^: (min maxLine . max 0)) .
        (secondA ^: (min maxCol . max 0))
        ) $
    buffer
  where
    maxLine, maxCol :: Integer
    maxLine = pred $ genericLength (buffer ^. contents)
    maxCol =
        genericLength ((buffer ^. contents) !! fromIntegral line)
    (line, col) =
        firstA ^: (max 0 . min maxLine) $
        (buffer ^. cursorPosition)

bufferFromFile :: MonadIO m => FilePath -> m Buffer
bufferFromFile file = liftIO $ do
    contents <- lines <$> readFile file
    return (Buffer (0, 0) (contents ++ [""]))

insertCharacter :: Char -> Buffer -> Buffer
insertCharacter c buffer =
    cursorPosition .> secondA ^: succ $
    contents ^: modByIndex line (insertByIndex col c) $
    buffer
  where
    (line, col) = buffer ^. cursorPosition

backspace :: Buffer -> Buffer
backspace buffer =
    if col <= 0 then
        if line <= 0 then
            buffer
          else
            cursorPosition ^:
                (((secondA ^= genericLength ((buffer ^. contents) !! fromIntegral (pred line)))) .
                 (firstA  ^= pred line)) $
            contents ^: mergeLines (pred line) $
            buffer
      else
        cursorPosition .> secondA ^: pred $
        contents ^: modByIndex line (deleteByIndex (pred col)) $
        buffer
  where
    (line, col) = buffer ^. cursorPosition

modByIndex :: Integer -> (a -> a) -> [a] -> [a]
modByIndex 0 f (a : r) = f a : r
modByIndex n f (a : r) = a : modByIndex (pred n) f r

deleteByIndex :: Integer -> [a] -> [a]
deleteByIndex 0 (_ : r) = r
deleteByIndex n (a : r) = a : deleteByIndex (pred n) r

insertByIndex :: Integer -> a -> [a] -> [a]
insertByIndex 0 e l = e : l
insertByIndex n e (a : r) = a : insertByIndex (pred n) e r

mergeLines :: Monoid a => Integer -> [a] -> [a]
mergeLines 0 (a : b : r) = a <> b : r
mergeLines n (a : r) = a : mergeLines (pred n) r

firstA :: Accessor (a, b) a
firstA = accessor fst (\ a (_, b) -> (a, b))

secondA :: Accessor (a, b) b
secondA = accessor snd (\ b (a, _) -> (a, b))
