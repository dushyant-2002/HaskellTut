{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Folds where
import Prelude hiding (flip, length, and, map, elem,foldr,filter,(++),take,reverse)



elem :: Eq a => a -> [a] -> Bool
elem  _ [] = False
elem  x (y : ys) = x == y || elem x ys

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs


and :: [Bool] -> Bool
and [] = True
and (x : xs) = x && and xs



fun :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
fun cons nil [] = nil
fun cons nil (x : xs) = cons x (fun cons nil xs)

--foldr (+) 0 [1,2,3]
-- = (+) 1 (fun (+) 0 [2,3])
-- = (+) 1 ((+) 2 (fun (+) 0  [3]))
-- = (+) 1 ((+) 2 ((+) 3 (fun (+) 0 [])))
-- = (+) 1 ((+) 2 ((+) 3 (0)))
-- = (+) 1 ((+) 2 (3))
-- = (+) 1 (5)
-- = 6


-- foldr :: Num t2 => (t1 -> t2 -> t2) -> [t1] -> t2
-- foldr f = fun f 0


-- foldr :: (a -> r -> r) -> r -> [a] -> r
foldr :: (t -> p -> p) -> p -> [t] -> p
foldr op e = go
    where
        go [] = e
        go (x : xs) = op x (go xs)

length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs



length' = foldr (\x r -> 1 + r) 0


-- ++ using foldr

-- (++) :: [a] -> [a] -> [a]
-- (++) xs ys = foldr (:) ys xs

-- filter :: (a -> Bool) -> [a] -> [a]
-- filter p  = foldr (\x r -> if p x then x : r else r) [] 

-- map' :: (a -> b) -> [a] -> [b]
-- map' f = foldr(\x r -> f x : r) []

-- and' :: [Bool] -> Bool
-- and' = foldr (&&) True



-- any :: (t -> Bool) -> [t] -> Bool
-- any f = foldr (\x r -> f x || r) False





-- foldl

funl :: (r -> a -> r) -> r -> [a] -> r
funl op e [] = e
funl op e (x:xs) = funl op (op e x) xs

--this is foldl
funl' :: (t -> r -> t) -> t -> [r] -> t
funl' op e = go e
    where
    go !acc []       = acc
    go !acc (x:xs)   = go (op acc x) xs


-- ++ using foldr

(++) :: [a] -> [a] -> [a]
(++) a b = foldr (:) b a


-- map using foldr
mapr :: (a -> b) -> [a] -> [b]
mapr f = foldr (\x r -> f x : r) []

filterr :: (a -> Bool) -> [a] -> [a]
filterr f = foldr (\x r -> if f x then x :r else r) []

andr :: [Bool] -> Bool
andr = foldr (&&) True


any :: (a -> Bool) -> [a] -> Bool
any f = foldr (\x r -> f x || r) False

--selft test

take :: Int -> [a] -> [a]
take n = foldr hlpr [ ]
 where
    hlpr x acc
        | length acc < n = x : acc
        | otherwise = acc


---foldl

foldl' :: (r -> a-> r) -> r -> [a] -> r
foldl' upd ini = go ini
    where
        go !acc [] = acc
        go !acc (x:xs) = go (upd acc x) xs


--reimplement functinos using foldl'
reverse ::[a] -> [a]
-- reverse = foldl' (\acc x -> x : acc) []
--reverse using flip and fold
reverse = foldl' (flip (:)) []

-- flip
flip :: (a-> b -> c) -> b -> a -> c
flip f a b = f b a

--sum using foldl

sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

--length using foldl
lengthl :: [a] -> Int
lengthl = foldl' (\acc _ -> acc +1) 0


--self task of foldl'

avg :: Fractional b => [b] -> b
avg  = hlper (0,0) 
    where
        hlper (a,b) [] = a/b
        hlper (a,b) (x:xs) = hlper (a+x,b+1) xs

--filter using foldl'
--foldl' accepts f acc x -> element of a list while foldr have element of a list first and then acc
filterl :: (a -> Bool) -> [a] -> [a]
filterl f = foldl' (\acc x -> if f x then x : acc else acc) []



--using uncurrying
uncurry :: (a->b ->c) -> (a,b)-> c
uncurry f (x,y) = f x y



