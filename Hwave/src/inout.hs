import Data.Maybe
import qualified Data.Map as Map

names :: Map.Map Int String
names = Map.fromList [(0, "Kekus"), (1, "Loli")]

helloPerson :: String -> String
helloPerson name = mconcat ["Hello ", name, "!"]

mainFn :: Int -> Maybe String
mainFn n = do
  name <- Map.lookup n names
  let greetings = helloPerson name
  return greetings

fastFib :: Int -> Int -> Int -> Int
fastFib n _ 0 = n
fastFib n o c = fastFib o (n + o) (pred c)

ffib :: Int -> Int
ffib n = fastFib 0 1 n

main :: IO ()
main = do 
  putStrLn "Enter number: "
  n <- getLine
  let fib = ffib (read n)
  putStrLn (show fib)
