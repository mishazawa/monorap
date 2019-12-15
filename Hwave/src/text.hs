{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as Tl
import qualified Data.Text.Lazy.IO as TIOl

separator = "\n" :: T.Text

tlines :: T.Text -> [T.Text]
tlines txt = T.splitOn separator txt

tunlines :: [T.Text] -> T.Text
tunlines ws = T.intercalate separator ws

highlight :: T.Text -> T.Text -> T.Text
highlight query text = T.intercalate match template
  where template = T.splitOn query text
        match    = mconcat ["{", query, "}"]

tprint :: T.Text -> IO ()
tprint t = do
  TIO.putStrLn t

-- T version

helloPerson :: T.Text -> T.Text
helloPerson name = mconcat ["Hello, ", name, "!!;)"]

greetings :: IO ()
greetings = do
  name <- TIO.getLine
  tprint (helloPerson name)

-- T lazy

ttoInts :: Tl.Text -> [Int]
ttoInts = map (read . Tl.unpack) . Tl.lines

summ :: IO ()
summ = do
  inputStream <-TIOl.getContents
  let numbers = ttoInts inputStream
  print (sum numbers)

main :: IO ()
main = do
  summ
