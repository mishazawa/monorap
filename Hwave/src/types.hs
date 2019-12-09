-- types
--

makeTriple :: a -> a -> b -> (a, a, b)
makeTriple x y z = (x, y, z)

type FirstName = String
type LastName = String
type MiddleName = String
type FullName = (FirstName, LastName)
type Age = Int
type Weight = Int

patientInfo :: FullName -> Age -> Weight -> String
patientInfo n a w = foldl (++) "" [name, " (", age, ", ", weight, ")"]
  where name = snd n ++ ", " ++ fst n
        age = show a ++ "yr."
        weight = show w ++ "kg."

data Sex = Male | Female
data RhType = Pos | Neg
data AboType = A | B | AB | O 
data BloodType = BloodType AboType RhType

data Name = Name FirstName LastName | NameWm FirstName MiddleName LastName

type Freq = Float
data Wave = Sin | Square | Triangle | Saw
data Osc = Osc Wave Freq

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWm f m l) = f ++ " " ++ m ++ " " ++ l

data Patient = Patient {
  , name::Name
  , sex::Sex
  , age::Age
  , weight::Weight
  , blood::BloodType
}
