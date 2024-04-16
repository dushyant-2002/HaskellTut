

import Prelude hiding (Eq(..),Ord(..))
import Text.Read

class Eq a where
    (==) :: a -> a -> Bool
    x == y = not( x /= y) -- if we write default definitions of both and dont declare instance then we'll got stuck insideloop eg- Rock ==Rock
    (/=) :: a -> a -> Bool
    x /= y = not (x == y) --default definition we dont need to declare instance of /= we'll get results from ==
    {-# MINIMAL  (==) | (/=) #-} -- 


instance Eq Bool where
    True    == True     = True
    False   == False    = True
    _       ==   _      = False

data Choice  = Rock | Paper | Scissors

--instance for show
instance Show Choice where
    show :: Choice -> String
    show Rock = "Rock"
    show Paper = "Paper"
    show Scissors = "Scissors"

instance Eq Choice where
    Rock        ==  Rock     = True
    Paper       ==  Paper    = True
    Scissors    ==  Scissors = True
    _           ==     _     =  False 

instance Eq a => Eq [a] where
    [] == [] = True
    (x : xs) == (y : ys) = x == y && xs == ys -- we can give this if we give an instance declaration on a coz to compare x and y . x n y should be instane of Eq

instance Eq (Maybe a) where
instance (Bounded a, Enum a, Eq b) => Eq (a -> b) where
  f1 == f2 =
    all (\ x -> f1 x == f2 x) [minBound ..]



class Eq a => Ord a where
    (<=) :: a -> a -> Bool

instance Ord Bool where
    False <= y = True --false is smallest in boolean
    True <= y  = y 

instance Ord a => Ord (Maybe a) where --first make an instance of maybe a in Eq class


strange :: String -> String
-- strange x = show (read x)  When you use them without specifying the type, Haskell cannot infer the intended type from the context, leading to ambiguity.


strange x  = show(read x :: Int)

f :: Int -> Int
f x = read (show x)
