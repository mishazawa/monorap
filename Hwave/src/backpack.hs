import Data.List
import Data.Semigroup 

type Cost = Int
type Weight = Int
type Priority = Double

data Item = Item Double Double Double deriving (Eq, Ord, Show)

getWeight :: Item -> Double
getWeight (Item _ w _) = w

compItems (Item _ _ a) (Item _ _ b) 
  | a < b = GT
  | a > b = LT
  | otherwise = EQ


costs :: [Double]
costs = [1, 2, 2, 4, 10]

weights :: [Double]
weights = [1, 1, 2, 12, 4]

items :: [Item]
items = sortBy compItems (zipWith (\c w -> Item c w (c / w)) costs weights)

collect :: Double -> [Item] -> [Item] -> [Item]
collect 0 bp _ = bp
collect _ bp [] = bp
collect limit backpack items = collect newLimit newBackpack rest
  where next = head items 
        rest = tail items
        newBackpack = if getWeight next <= limit
                      then backpack <> next
                      else backpack
        newLimit = if getWeight next <= limit
                   then limit - getWeight next
                   else 0
