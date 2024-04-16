import Prelude hiding (Left, Right, Either, map, fst)
import Text.XHtml (paragraph)


-- fst :: (Int,Bool) -> Int
-- fst (x,_) = x



-- fst :: ([a],Char) -> [a]
-- fst (a,b) = a
-- most general type signature and assumed by ghci  can work on any types
fst :: (a,b) ->a
fst (a,_) = a 


-- no run time type information

restrictedfst :: (Int,Int) -> Int
restrictedfst = fst --ok

-- newfst :: (a,b) -> a
-- newfst = restrictedfst  --type error


loop :: a
loop = loop


--possible combinations of a function using strict type
-- (Int,Int) -> (Int , Int)
f1 :: (Int,Int) -> (Int,Int)
f1 (x,y) = (x*y,x+y)

f2 :: (Int,Int) -> (Int,Int)
f2 (x,y) = (32,x*y)

f3 :: (Int,Int) -> (Int,Int)
f3 (x,y) = (x*y+7,x+y*x+7)

---and virtually infine choices

--but with general type we dont have much choices coz we cannot use ==,+,/ etc in general type
--disadvantage of genreal type
g1 :: (a,b) -> (a,b)
g1 (x,y)  = (x,y)

g2 :: (a,b) -> (b,a)
g2 (x,y)  = (y,x)


map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

------- Union Types

-- parse :: String -> a
-- parse "True" = True
-- parse "0" = 0
--What if we need return values of different types

data Either a b = Left a | Right b
    deriving (Show,Eq)


--1.
-- parse :: String -> Either Bool Int
-- parse "True" = Left True
-- parse "0" = Right 0

--2. for more than two
--not much meaning full in practice
-- parse :: String -> Either (Either Bool Int) String
-- parse "True" = Left(Left True)
-- parse "0" = Left (Right 0)
-- parse  _ = Right "Unknown"


-- good choice would be to make your own data type
data Parseresult = ABool Bool | AInt Int | AString String
    deriving (Show,Eq)
parse :: String -> Parseresult
parse "True" = ABool True
parse "0" = AInt 0
parse _ = AString "Unknown"





-- data MyType a b = Mbool a | MInt b
--     deriving (Show,Eq)
-- parse :: String -> MyType Bool Int
-- parse "True" = Mbool True
-- parse "0" = MInt 0





