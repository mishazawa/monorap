import Data.List.Split

toInts :: String -> [Int]
toInts = map read . lines

squareSum :: [Int] -> Int
squareSum vals = sum squares
  where squares = map (^2) vals

main :: IO ()
main = do
  input <- getContents
  let values = toInts input
  print (squareSum values)
