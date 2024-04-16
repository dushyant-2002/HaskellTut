module Functions where

import Prelude hiding (length,elem,map,drop,(++),reverse,filter,take,fst,zip)

length :: [a] -> Int
length [] = 0
length (x : a) = 1 + length a


--implementing elem


elem :: Eq t => t -> [t] -> Bool
elem x [] = False
elem x (y:ys) = x == y || elem x ys


--implementing map

map :: (t -> a) -> [t] -> [a]
map f [] = []
map f (x : xs) = (f x): map f xs


--implementing drop

drop n [] = []
drop 0 xs = xs
-- drop n (x : xs ) =
--     if n <= 0
--         then x : xs
--     else drop (n-1) xs
--can use guards
drop n (x : xs)
    | n <= 0 = x : xs
    | otherwise = drop (n-1) xs

--append two lists

(++) :: [a] -> [a] -> [a]
[] ++ [] = []
[] ++ (y:ys) = y : ys
(x:xs) ++ ys = x : (xs ++ ys)

--Reverse a list

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ (x:[])


--Filter

filter :: (a -> Bool)-> [a] -> [a]
filter f [] = []
--Using if-else
-- filter f (x : xs) = if f x then x : filter f xs
--                                    else filter f xs
--Using Guards
-- filter f (x:xs)
--     | f x = x : filter f xs
--     |otherwise = filter f xs
--Using Case
filter f (x:xs) = 
    case f x of
        False -> filter f xs
        True -> x : filter f xs

--Self Test

--- take 

take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x:xs)
    | n <= 0 = []
    | otherwise = x:take (n - 1) xs

--Slice

slice :: Int -> Int -> [a] -> [a]
slice n1 n2 a = take n2 (drop n1 a)


--Snoc

snoc :: [a] -> a -> [a]
--One Liner using ++
-- snoc xs x = xs ++ (x:[])
snoc [] y = y :[]
snoc (x : xs) y = x : (snoc xs y)


--Selecting first Component of pair
fst :: (a,b)-> a
fst (a,b) = a

--Zipping Two List

zip :: [a] -> [b] -> [(a,b)]
zip [] [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys


 

