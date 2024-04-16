-- import Prelude hiding (not,(||),elem)

-- --can use five anywhere
-- ten = five + five

-- five = 2 + 3

-- aList = [1,2,3,4,five]

-- -- distance :: Num a => a -> a -> a ->Bool
-- distance :: (Eq a, Num a) => a -> a -> Bool --type signature
-- distance x y = abs(x - y) == 0


-- double x = x + x
-- triple x = double x + x
-- -- Defining a datatype

-- data Choice = Rock | Paper | Scissors
--     deriving Show

-- --Pattern Matching  
-- improve :: Choice -> Choice
-- --Searches  for Rock in Choice and then return Paper
-- improve Rock = Paper
-- improve Paper = Scissors
-- improve Scissors = Rock


-- worsen :: Choice -> Choice

-- worsen Rock = Scissors
-- worsen Paper = Rock
-- worsen Scissors = Paper




-- --Pattern Martching
-- --Ambiguous Reference error coz not is already present in prelude

-- -- not :: Bool -> Bool
-- -- not False = True
-- -- not True = False

-- myNot :: Bool -> Bool
-- myNot False = True
-- myNot True = False

-- --Implementing OR ||

-- -- (||) :: Bool -> Bool -> Bool
-- -- Method 1
-- -- False || False = False
-- -- False || True = True
-- -- True || False = True
-- -- True || True = True
-- --Method 2
-- -- (||) False False = False
-- -- (||) _ _ = True
-- --Method 3
-- -- False || y = y
-- -- True || y = True

--Part 1 List
---Our own list
-- {-# OPTIONS_GHC -Wno-overlapping-patterns #-}
-- import Prelude hiding (elem,(&&))
-- data List a = Nil | Cons a (List a)
--     deriving Show

-- sampleList :: List Int
-- sampleList = Cons 1(Cons 2 (Cons 3 Nil))

-- --Pattern matching in list
-- --1. acc to internal str
-- elem' :: Eq a => a -> List a -> Bool
-- elem' x Nil = False
-- -- elem' x (Cons y ys) = x==y --here we are only checking first element getting false if element is not matching with first. 
-- --For checking whole list we should use recursion
-- elem' x (Cons y ys) = x == y || elem' x ys

-- --2. Syntactical sugar
-- elem :: Eq t => t -> [t] -> Bool
-- elem x [] = False
-- elem x (y : ys) = x ==y || elem x ys



--Self Tests
--Pattern Matching
import Prelude hiding (elem,(&&))
import Data.Text.Array (equal)
-- 1.&& Operator
(&&) :: Bool -> Bool -> Bool

True && True = True
_ && _ = False

--2. List
data List b = Nil | Cons b (List b)
    deriving Show

sampleList :: List Int
sampleList = Cons 1(Cons 2 (Cons 3 Nil))

elem' :: Eq t => t -> List t -> Bool
elem' x Nil = False
elem' x (Cons y ys) = x == y || elem' x ys


elem x [] = False
elem x (y : ys) = x == y || elem x ys

--3. all elements are equal in list or notenu
equal' :: Eq t => t -> [t] -> Bool
equal' x [] = False
equal' x (y : ys) = (x == y) && (equal' x ys)









