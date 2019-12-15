{-# LANGUAGE OverloadedStrings #-}

import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as Tio

getCounts :: T.Text -> (Int, Int, Int)
getCounts file = (c, w, l)
  where c = T.length file
        w = (length . T.words) file
        l = (length . T.lines) file

formatCounts :: (Int, Int, Int) -> T.Text
formatCounts (c, w, l) = T.pack (unwords v)
  where v = ["chars:", show c, "; words:", show w, "; lines:", show l]

analysis :: FilePath -> IO ()
analysis fname = do
  input <- Tio.readFile fname
  let summary = (formatCounts . getCounts) input
  Tio.putStrLn (T.intercalate " -- " [(T.pack fname), summary])

