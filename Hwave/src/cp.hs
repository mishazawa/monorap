{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as Tio

main :: IO ()
main = do
  args <- getArgs
  let (source:destination:_) = args
  
  input <- Tio.readFile source
  Tio.writeFile destination input
