import Prelude hiding (map, elem)

elem :: Eq a => a -> [a] -> Bool
elem - []  = False
elem y (x:xs) = y == x || elem y xs


-- If x == y
-- elem x (y:ys)
-- = x == y || elem x ys
-- = True || elem x ys
-- = True


--  if x /= y
-- elem x (y:ys)
-- x == y || elem x ys
-- False || elem x ys
-- = elemm x ys



-- elem using acc

elemAcc :: Eq a => Bool -> a -> [a] -> Bool
elemAcc !acc _ [] = acc
elemAcc !acc x (y:ys) = elemAcc (acc || x == y) x ys

elem' :: Eq a => a -> [a] -> Bool
elem' = elemAcc False


--getting stack overflow 
--reason acc becomes like False||FAlse ||False || False ----------105
--we are having a large no of || operations in the stack coz we are not using bang patterns here thats why it causing stack overflow
--use !acc in place of acc to avoid stack overflow


--map

map :: ( a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = (f x) : map f xs


mapAcc:: [b] -> (a -> b) -> [a] -> [b]
mapAcc acc f [] =  acc
mapAcc acc f (x:xs) = mapAcc (acc ++ [f x]) f xs
-- mapAcc !acc f [] =  reverse acc
-- mapAcc !acc f (x:xs) = mapAcc ((f x) : acc) f xs

map' :: (a->b) -> [a] -> [b]
map' = mapAcc []