import Data.List
import Data.Semigroup
import Data.Maybe
import qualified Data.Map as Map

type Point = (Int, Double)

file1 :: [Point]
file1 = [(1, 200.1), (2,  199.5), (3, 199.4)
        , (4, 198.9), (5,  199.0), (6, 200.2)
        , (9, 200.3), (10, 201.2), (12, 202.9)]

file2 :: [Point]
file2 = [(11, 201.6), (12, 201.5), (13, 201.5)
        ,(14, 203.5), (15, 204.9), (16, 207.1)
        ,(18, 210.5), (20, 208.8)] 

file3 :: [Point]
file3 = [(10, 201.2), (11, 201.6), (12, 201.5)
        ,(13, 201.5), (14, 203.5), (17, 210.5)
        ,(24, 215.1), (25, 218.7)] 

file4 :: [Point]
file4 = [(26, 219.8), (27, 220.5), (28, 223.8)
        ,(29, 222.8), (30, 223.8), (31, 221.7)
        ,(32, 222.3), (33, 220.8), (34, 219.4)
        ,(35, 220.1), (36, 220.6)]

data TimeSeries a = TimeSeries [Int] [Maybe a]

createTimeSeries :: [Int] -> [a] -> TimeSeries a
createTimeSeries times values = TimeSeries completeT extendedV
  where completeT = [minimum times .. maximum times]
        tvMap     = Map.fromList (zip times values)
        extendedV = map (\v -> Map.lookup v tvMap) completeT

fileToTimeSeries :: [(Int, a)] -> TimeSeries a
fileToTimeSeries file = createTimeSeries times values
  where (times, values) = unzip file

showTvPair :: Show a => Int -> (Maybe a) -> String
showTvPair time (Just v) = mconcat [show time, "|", show v, "\n"]
showTvPair time Nothing  = mconcat [show time, "|N/A\n"]

instance Show a => Show (TimeSeries a) where
  show (TimeSeries times values) = mconcat rows
    where rows = zipWith showTvPair times values

insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair m (_, Nothing) = m
insertMaybePair m (k, (Just v)) = Map.insert k v m

combineTimeSeries :: TimeSeries a -> TimeSeries a -> TimeSeries a
combineTimeSeries (TimeSeries [] []) ts2 = ts2
combineTimeSeries ts1 (TimeSeries [] []) = ts1
combineTimeSeries (TimeSeries t1 v1) (TimeSeries t2 v2) = TimeSeries completeT extendedV
  where bothT = mconcat [t1, t2]
        completeT = [minimum bothT .. maximum bothT]
        tvMap     = foldl insertMaybePair Map.empty (zip t1 v1)
        updMap    = foldl insertMaybePair tvMap (zip t2 v2)
        extendedV = map (\v -> Map.lookup v updMap) completeT

instance Semigroup (TimeSeries a) where
  (<>) = combineTimeSeries

instance Monoid (TimeSeries a) where
  mempty = TimeSeries [] []
  mappend = (<>)

mean :: (Real a) => [a] -> Double
mean xs = total / count
  where total = (realToFrac . sum) xs
        count = (realToFrac . length) xs

meanTimeSeries :: (Real a) => TimeSeries a -> Maybe Double
meanTimeSeries (TimeSeries _ []) = Nothing
meanTimeSeries (TimeSeries t v) = if all (== Nothing) v
                                  then Nothing
                                  else Just avg
  where values = ((map fromJust) . (filter isJust)) v
        avg = mean values

type CompareFn a = a -> a -> a
type TimeSeriesCompareFn a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

compareTimeSeries :: Eq a => CompareFn a -> TimeSeriesCompareFn a
compareTimeSeries fn = fn'
  where fn' (i1, Nothing) (i2, Nothing) = (i1, Nothing)
        fn' (_, Nothing) (i, val) = (i, val)
        fn' (i, val) (_, Nothing) = (i, val)
        fn' (i1, Just val1) (i2, Just val2) = if fn val1 val2 == val1
                                              then (i1, Just val1)
                                              else (i2, Just val2)
