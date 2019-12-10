-- type classes

class Describable a where
  describe :: a -> String

data IceCream = Vanilla | Chocolate deriving (Show, Eq, Ord)

data FiveSideDie = S1 | S2 | S3 | S4 | S5 deriving (Show, Eq)

class Die a where
  isOne :: a -> Bool

instance Die FiveSideDie where
  isOne d = d == S1

rotateN::(Bounded a, Enum a) => Int -> a -> a
rotateN n c = toEnum x
  where halfN  = n `div` 2
        offset = if even n 
                 then fromEnum c + halfN
                 else 1 + fromEnum c + halfN 
        x      = offset `mod` n 

encoder :: String -> String
encoder sentense = map encode sentense 
  where size = 1 + fromEnum (maxBound::Char)
        encode = rotateN size

decoder code = encoder code

xxor :: Bool -> Bool -> Bool
xxor a b = (a || b) && (not (a && b))

xxorp :: (Bool, Bool) -> Bool
xxorp (a, b) = xxor a b

xxorl :: [Bool] -> [Bool] -> [Bool]
xxorl a b = map xxorp (zip a b)

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if (rem == 0) 
              then False : intToBits' next
              else True  : intToBits' next
  where rem = n `mod` 2
        next = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits n = padding ++ reversed
  where reversed = reverse (intToBits' n)
        missing = maxBits - (length reversed)
        padding = take missing (cycle [False])

charToBits :: Char -> Bits
charToBits c = intToBits (fromEnum c)

bitsToInt :: Bits -> Int
bitsToInt b = sum (map (\x -> 2^(snd x)) trues)
  where size = length b
        ind  = [size - 1, size - 2 .. 0]
        trues = filter (\v -> fst v == True) (zip b ind)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

encodeStr :: String -> String
encodeStr str = map encode str
  where encode = (toEnum charToBits) 


