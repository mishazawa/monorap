-- Greedy algorithm knapsack problem

import Data.List

type Cost = Int
type Weight = Int
type Priority = Double

data Item = Item Cost Weight Priority deriving (Eq, Ord, Show)

type Backpack = [Item]
type Items    = [Item]

getWeight :: Item -> Weight
getWeight (Item _ w _) = w

calcPriority :: Cost -> Weight -> Priority
calcPriority c w = (fromIntegral c) / (fromIntegral w)

newItem :: Cost -> Weight -> Item
newItem c w = Item c w (calcPriority c w)

compItems (Item _ _ a) (Item _ _ b) = compare a b

costs :: [Cost]
costs = [1, 2, 2, 4, 10]

weights :: [Weight]
weights = [1, 1, 2, 12, 4]

items :: Items
items = sortBy compItems (zipWith newItem costs weights)

collect :: Int -> Backpack -> Items -> Backpack
collect 0 bp _ = bp
collect _ bp [] = bp
collect limit backpack (next:rest) = collect newLimit newBackpack rest
  where weight = getWeight next
        (newBackpack, newLimit) = if weight <= limit
                                  then (next:backpack, limit - weight)
                                  else (backpack, 0)

