import Data.List.Split

simpleCalc :: Int -> Int -> [Char] -> Int
simpleCalc a b "+" = a + b
simpleCalc a b "*" = a * b
simpleCalc a b "-" = a - b
simpleCalc a b "/" = a `div` b

main :: IO ()
main = do
  input <- getLine
  let (a:op:b:_) = splitOneOf " \n" input
  print (simpleCalc (read a) (read b) op)
  main
