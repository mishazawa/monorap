import qualified Data.Map as Map

reverseMaybe :: Maybe String -> Maybe String
reverseMaybe = fmap reverse

data RobotPart = RobotPart {
    name :: String
  , description :: String
  , cost :: Double 
  , count :: Int
} deriving Show

type Html = String

renderHtml :: RobotPart -> Html
renderHtml p = mconcat ["<h2>", pName, "</h2>",
                      "<p><h3>", pDesc, "</h3></p>",
                      "<p><h3>", pCost, "</h3></p>",
                      "<p><h3>", pCount, "</h3></p>"]
  where pName = name p
        pDesc = description p
        pCost = show (cost p)
        pCount = show (count p)

leftArm :: RobotPart
leftArm = RobotPart {
    name = "left arm"
  , description = "left arm for face punching!"
  , cost = 1000.00
  , count = 3
}
rightArm :: RobotPart
rightArm = RobotPart {
    name = "right arm"
  , description = "right arm for kind hand gestures"
  , cost = 1025.00
  , count = 5
}
robotHead :: RobotPart
robotHead = RobotPart {
    name = "robot head"
  , description = "this head looks mad"
  , cost = 5092.25
  , count = 2
}

partsDb :: Map.Map Int RobotPart
partsDb = Map.fromList kparts
  where k = [1, 2, 3]
        p = [leftArm, rightArm, robotHead]
        kparts = zip k p

htmlPartsDb :: Map.Map Int Html
htmlPartsDb = renderHtml <$> partsDb

data Box a = Box a deriving Show

instance Functor Box where
  fmap func (Box val) = Box (func val)

morePresents :: Int -> Box a -> Box [a]
morePresents n val = fmap (copies n) val
  where copies c i = (take c . repeat) i

unwrap :: Box a -> a
unwrap (Box a) = a
