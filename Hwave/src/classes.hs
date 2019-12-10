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

data Alph = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Bounded, Enum, Show)

alphabetEncoder :: [Alph] -> [Alph]
alphabetEncoder letters = map encode letters
  where alphSize = 1 + fromEnum (maxBound::Alph)
        encode = rotateN alphSize
