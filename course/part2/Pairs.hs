

import Prelude hiding (fst,uncurry,swap,zip,lookup)
import Data.Map (Map)
import Control.Applicative




data Pair  a b = Pair a b
    deriving Show

fst' :: Pair a b -> a

-- fst (Pair x _) = x 

--Pair have only single constructor so we can also define it usinf lambda function

fst' = (\(Pair x _)-> x) 


fst :: (a,b) -> a
fst (a,_) = a

--Currying

plus :: (Int,Int) -> Int
-- plus (a,b) = a + b
plus (a,b) = uncurry (+) (a,b)

uncurry :: (a -> b -> c) -> (a , b) -> c
uncurry f (x,y) = f x y

swp :: (a,b) -> (b,a)
swp (a,b) = (b,a)


--Zippint Two Lists

zip :: [a] -> [b] -> [(a,b)]
zip [] []               = []
zip [] _                = []
zip _ []                = []
zip (x : xs) (y : ys)   = (x,y) : zip xs ys



--Lookup Function

example :: [(Int,String)]
example = [
    (1,"Frodo")
    ,(2,"Rj")
    ,(3,"Rahul")
    ,(4,"Dushyant")
    ,(5,"Dhruv")
    ,(1,"Ayush")
    ]




-- lookup :: Eq key => key -> [(key,val)] -> val --Eq key coz we have to use equality operator 
-- lookup _ [] = error "Unknown key" --on empty list our code get crashed but we should avoid crashing
-- lookup key ((key',val) : table) = if key == key' then val
--                                   else lookup key table
-- lookup key ((key',val) : table ) 
--     | key == key' = val
--     | otherwise = lookup key table


--for better error handling without crashing
-- data LookupResult val = LookupFailed | LookupSuccessfull val 
--     deriving Show
-- lookup :: Eq key => key -> [(key,val)] -> LookupResult val 
-- lookup _ [] = LookupFailed
-- lookup key ((key',val) : table) = if key == key' then LookupSuccessfull val
--                                     else lookup key table


--Using Maybe
lookup :: Eq key => key -> [(key,val)] -> Maybe val
lookup _ []                     = Nothing
lookup key ((key',val):table)
    | key == key'               = Just val
    | otherwise                 = lookup key table



--Recovering a value from maybe

fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing   = def
fromMaybe _   (Just x)  = x 

-- ghci> fromMaybe "Unknow key" (lookup 3 example)
-- "Rahul"

--ghci> fromMaybe "Unknown key" (lookup 30 example)
-- "Unknown key"

--mapMaybe
mapMaybe :: (a -> b) -> Maybe a -> Maybe b -- ---> availaible more generally as fmap or <$>
mapMaybe _ Nothing = Nothing
mapMaybe f (Just x) = Just(f x)
--ghci> mapMaybe (+1) (Just 5)
-- Just 6

--orelse for fallback source

orelse :: Maybe a -> Maybe a -> Maybe a -- -----> availaible as <|> in Control.Applicative module
orelse Nothing y  = y 
orelse (Just x) _ = Just x

example' :: [(Int,String)]
example' = 
    [(17,"Gollum")
    ]





