type FirstName  = String
type LastName   = String
type MiddleName = String

data Name = Name FirstName LastName
          | NameWm FirstName MiddleName LastName
          | InitialsName Char Char LastName
          | NameInitials FirstName Char Char

data Author = Author Name

-- Unfortunately, real life is full of strange edge 
-- cases that make this much more complicated than you’d typically want.

data Artist = Person Name | Band String
data Creator = AuthorCreator Author | ArtistCreator Artist

data Book = Book {
    author :: Creator
  , isbn   :: String
  , bookTitle :: String
  , bookYear  :: Int
  , bookPrice :: Double
}

data Vinyl = Vinyl {
    artist :: Creator
  , recordTitle :: String
  , recordYear :: Int
  , recordPrice :: Double
}

data Toy = Toy {
    name :: String
  , toyDescription :: String
  , toyPrice :: Double
}

data Pamphlet = Pamphlet {
    title :: String
  , pampDescription :: String
  , contacts :: String
}

data StoreItem = BookItem Book
               | RecordItem Vinyl
               | ToyItem Toy 
               | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem pamp) = 0

data Circle = Circle {
  radius :: Double
}

data Square = Square {
  side :: Double
}

data Rectangle = Rectangle {
    sideA :: Double
  , sideB :: Double
}

data Shape = ShapeCircle Circle
           | ShapeSquare Square 
           | ShapeRectangle Rectangle

perimeter :: Shape -> Double
perimeter (ShapeCircle c) = 2 * 3.1415 * radius c
perimeter (ShapeSquare s) = 4 * side s
perimeter (ShapeRectangle r) = 2 * sideA r + 2 * sideB r

area :: Shape -> Double
area (ShapeCircle c) = 3.1415 * radius c ^ 2
area (ShapeSquare s) = side s ^ 2
area (ShapeRectangle r) = sideA r * sideB r
