import Prelude hiding (length, null, sum, take, drop, map, filter, foldr)


--A1
data Choice = Rock | Paper | Scissors
  deriving Show

isPaper :: Choice -> Bool
isPaper Paper = True
isPaper _ = False

--A2

null :: [a] -> Bool
null [] = True
null _ = False

---A3
-- equality operator is in eq class hence equality(==) works only when its an instance of eq class
--In null3 we have to count length of xs by traversing whole list which takes O(n) time complexity
--but in pattern matching we are just matching pattern and getting result in O(1)

--A4
length :: [a] -> Int
length [] = 0
length (_:a) = 1 + length a

--A5
noPaper :: [Choice] -> Bool
noPaper [] = True
noPaper (x:a) = not (isPaper x) && noPaper a

--A6

sum :: [Int] -> Int
sum [] = 0
sum (x:a) = x + sum a

--A7

data List b = Nil | Cons b (List b)
  deriving Show

from :: List b -> [b]
from Nil = []
from (Cons y b) = y : from b

to :: [b] -> List b
to [] = Nil
to (x:b) = Cons x (to b)

--A8

evens :: [Int] -> [Int]
evens [] = []
evens (x : a) = if even x then x : evens a else evens a
-- evens (x : a) = if x `mod` 2 == 0 then x : evens a
-- else evens a

--A9

sumEvenSquares :: [Int] -> Int
sumEvenSquares [] = 0
sumEvenSquares (x : a) =
    if even x then x*x + sumEvenSquares a
    else sumEvenSquares a

--A10

allEven :: [Int] -> Bool
allEven [] = True
allEven (x : a) = even x && allEven a

--A11
-- next :: Num a => [a] -> a
-- next [] = 100
-- next (x : a) = x
-- isAscending :: [Int] -> Bool
-- isAscending [] = True
-- isAscending (x : a) = (x < next a ) && isAscending a

isAscending :: [Int] -> Bool
isAscending [] = True
isAscending [_] = True 
isAscending (x : y : a) = (x <= y) && isAscending (y : a) --include y in list and then make recursive call



--A12
data Nat = Zero | Suc Nat
    deriving Show

fromNat :: Num a => Nat -> a
fromNat Zero = 0
fromNat (Suc a) = 1 + fromNat a

--A13
add:: Nat -> Nat -> Nat
add Zero b = b -- a goes to Zero then simply append b
add (Suc a) b = Suc (add a b) --first expand a 



