import Data.Semigroup

data Color = Red
           | Green
           | Blue
           | Purple
           | Yellow
           | Orange
           | White
           | Black
           | Brown deriving (Show, Eq)

instance Semigroup Color where
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) a b | a == b = a
           | a == White = b
           | b == White = a
           | any (== Black) [a, b] = Black
           | all (`elem` [Red, Blue, Purple])   [a, b] = Purple
           | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
           | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
           | all (`elem` [Red, Green, Blue])    [a, b] = White
           | otherwise = Brown

instance Monoid Color where
  mempty = White
  mappend = (<>)

cartesianComb :: (a -> b-> c) -> [a] -> [b] -> [c]
cartesianComb fn l1 l2 = zipWith fn newl1 cycledl2
  where n = length l2
        repeated = map (take n . repeat) l1
        newl1 = mconcat repeated
        cycledl2 = cycle l2

data Events = Events [String] deriving (Show)

combineEvents :: Events -> Events -> Events
combineEvents (Events e1) (Events e2) = Events (cartesianComb combiner e1 e2)
  where combiner = (\x y -> mconcat [x, "-", y])

instance Semigroup Events where 
  (<>) e1 e2 = combineEvents e1 e2 

instance Monoid Events where
  mempty = Events []
  mappend = (<>)

data Probs = Probs [Double] deriving (Show)

combineProbs :: Probs -> Probs -> Probs
combineProbs (Probs p1) (Probs p2) = Probs (cartesianComb (*) p1 p2)

instance Semigroup Probs where 
  (<>) p1 p2 = combineProbs p1 p2

instance Monoid Probs where
  mempty = Probs []
  mappend = (<>)

