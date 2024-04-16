module MyLib where
import Prelude hiding (mapM,sequence)
-- import Data.Map.Strict (Map) 
import Control.Applicative --for liftA2
import qualified Data.Map.Strict as M
import Control.Monad hiding (sequence,mapM)

table :: M.Map Int Int 
table =
    M.fromList
    [(1,2)
    ,(2,4)
    ,(3,6)
    ,(4,8)
    ,(5,10)
    ,(6,12)
    ,(8,16)
    ,(10,20)
    ,(15,30)
    ,(16,32)
    ,(20,40)
    ,(32,64)
    ]



--Video 2 :- Sequencing Possibly Failing Computations
--Task 
--Starting from a given integer , we want to perform three successive 
--lookups in our table (By successive , we mean that the result of 
-- one lookups serves as an input for the next) then we return final result incremented by 1

--if any of the lookup is failed then our whole operation is failed

threeLookups :: Int -> Maybe Int
threeLookups i0 =
    case M.lookup i0 table of
        Nothing -> Nothing
        Just i1 -> case M.lookup i1 table of
            Nothing -> Nothing
            Just i2 -> case M.lookup i2 table of
                Nothing -> Nothing
                Just i3 -> Just (i3+1)

--here we are writing same lines with minor differences
--to abstract from this pattern we have two methods
---1. use recursion with an argument n which stores how many repetitions are there
--2. using bind operator we created for Maybe

-- (>>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
-- (>>>=) comp rest =
--     case comp of
--         Nothing -> Nothing
--         Just x -> rest x



-- threeLookups2 :: Int -> Maybe Int
-- threeLookups2 i0 =  
--     M.lookup i0 table >>>= \i1 ->
--     M.lookup i1 table >>>= \i2 ->
--     M.lookup i2 table >>>= \i3 ->
--     Just (i3+1)

-- -- someLookups 
-- someLookups :: Int -> Int -> Maybe Int
-- someLookups 1 k = case M.lookup k table of
--     Nothing -> Nothing
--     Just x -> Just (x+1)
-- someLookups n k  = case M.lookup k table of
--     Nothing -> Nothing
--     Just x  -> someLookups (n-1) x


---------------Video 3 Monad Class ---------

threeLookups2 :: Int -> Maybe Int
threeLookups2 i0 =  
    M.lookup i0 table >>= \i1 ->
    M.lookup i1 table >>= \i2 ->
    M.lookup i2 table >>= \i3 ->
    return (i3+1)
--we can use >>= coz Maybe is also instance of monad class in which bind is defined and can use return in place of just
-- return :: a -> m a
-- >> 
threeLookups3 :: Int -> Maybe Int
threeLookups3 i0 = do 
    i1 <- M.lookup i0 table
    i2 <- M.lookup i1 table
    i3 <- M.lookup i2 table
    return (i3+1)

---- previosly we had used sequence for
-- sequence :: [IO a] -> IO [a]
-- sequence :: [Maybe a] -> Maybe [a]
-- it also present in monad and can be used with every instance of monad class
--sequence [Just 3, Just 4, Just 7, Just 101] ---O/p -Just [3,4,7,101]
--sequence [Just 3, Just 4, Just 7, Just 101,Nothing]   --O/p - Nothing
--one failing computation leads to overall failing computation




----Video 4   Functions for Free-----
 
--creating lookup
-- lookups' :: Eq a => [a] -> [(a,b)] -> Maybe [b]
-- lookups' keys tbl =
--     mapM (\key -> M.lookup key tbl ) keys





lookups :: Ord a => [a] -> M.Map a b -> Maybe [b]
lookups keys tbl = 
    mapM (\key -> M.lookup key tbl ) keys

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f 

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (x : xs) = liftA2 (:) x (sequence xs)
-- sequence (x :xs) = do
--     a <- x
--     as <- sequence xs
--     return (a : as) 


