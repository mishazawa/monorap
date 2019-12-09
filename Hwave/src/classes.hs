-- type classes

class Describable a where
  describe :: a -> String

data IceCream = Vanilla | Chocolate deriving (Show, Eq, Ord)

