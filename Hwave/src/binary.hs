import System.Environment
import System.Random
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

intToChar :: Int -> Char
intToChar n = toEnum (n `mod` 255)

intToBc :: Int -> BC.ByteString 
intToBc n = BC.pack [intToChar n]

replaceByte :: BC.ByteString -> Int -> Int -> BC.ByteString
replaceByte bytes location char = mconcat [before, replaced, after]
  where (before, rest) = BC.splitAt location bytes
        after = BC.drop 1 rest
        replaced = intToBc char

randReplaceByte :: BC.ByteString -> IO BC.ByteString
randReplaceByte bytes = do
  let len = BC.length bytes
  loc    <- randomRIO (1, len)
  char   <- randomRIO (1, 255)
  return (replaceByte bytes loc char)

sortBytes :: BC.ByteString -> Int -> Int -> BC.ByteString
sortBytes bytes start len = mconcat [before, replaced, after]
  where (before, rest) = BC.splitAt start bytes
        (range, after) = BC.splitAt len rest
        replaced       = BC.reverse (BC.sort range)

-- todo rwr more generic

randSortBytes :: BC.ByteString -> IO BC.ByteString
randSortBytes bytes = do
  let len   =  BC.length bytes
  sortRange <- randomRIO (10, 110)
  location  <- randomRIO (0, len - sortRange)
  return (sortBytes bytes location sortRange)

reverseBytes :: BC.ByteString -> Int -> Int -> BC.ByteString
reverseBytes bytes start len = mconcat [before, replaced, after]
  where (before, rest) = BC.splitAt start bytes
        (range, after) = BC.splitAt len rest
        replaced       = BC.reverse range


randReverseBytes :: BC.ByteString -> IO BC.ByteString
randReverseBytes bytes = do
  let len   =  BC.length bytes
  sortRange <- randomRIO (10, 110)
  location  <- randomRIO (0, len - sortRange)
  return (reverseBytes bytes location sortRange)

applyEffects :: [BC.ByteString -> IO BC.ByteString] -> BC.ByteString -> IO BC.ByteString
applyEffects effects image = do
  foldM (\bytes f -> f bytes) image effects 

glitch :: FilePath -> (BC.ByteString -> IO BC.ByteString) -> IO ()
glitch fpath fn = do
  imageFile <- BC.readFile fpath
  glitched <- fn imageFile
  let fname = mconcat ["g_", fpath]
  BC.writeFile fname glitched

main :: IO ()
main = do
  args <- getArgs
  glitch (head args) (applyEffects [
    randReplaceByte,
    randSortBytes,
    randSortBytes,
    randReplaceByte,
    randReverseBytes,
    randReplaceByte,
    randSortBytes,
    randReverseBytes,
    randReplaceByte,
    randReplaceByte,
    randReplaceByte,
    randSortBytes,
    randReplaceByte,
    randReverseBytes,
    randReplaceByte,
    randSortBytes,
    randSortBytes,
    randSortBytes,
    randReplaceByte,
    randReverseBytes,
    randReplaceByte,
    randSortBytes,
    randSortBytes,
    randReverseBytes,
    randSortBytes,
    randSortBytes,
    randSortBytes,
    randReverseBytes,
    randSortBytes,
    randReplaceByte,
    randReverseBytes,
    randReplaceByte,
    randReverseBytes,
    randReplaceByte,
    randReplaceByte,
    randSortBytes,
    randSortBytes,
    randReverseBytes,
    randSortBytes,
    randSortBytes,
    randSortBytes,
    randReverseBytes,
    randSortBytes,
    randSortBytes,
    randReverseBytes,
    randReplaceByte,
    randSortBytes])

