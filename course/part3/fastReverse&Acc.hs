{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

import Prelude hiding (reverse,sum,length)

slowReverse :: [a] -> [a]
slowReverse [] = []
slowReverse (x:xs) = slowReverse xs ++ [x]


reverseAcc :: [a] -> [a] -> [a]
reverseAcc acc [] = acc
reverseAcc acc (x : xs) = reverseAcc (x:acc) xs


reverse :: [a] -> [a]
reverse = reverseAcc [] --ETA Reduction


sum :: [Int] -> Int
sum [] = 0
sum (x : xs) = x + (sum xs)



--Using accucmulator in sum

sumAcc :: Int -> [Int] -> Int
sumAcc !acc [] = acc
sumAcc !acc (x:xs) = sumAcc (x+acc) xs

sum' :: [Int] -> Int
sum' = sumAcc 0



--sum' [1,2,3]
--sumAcc 0 [1..3]
--sumAcc 0 (1:[2..3])
--sumACC (0+1) [2,3]
--sumAcc (0+1) 2:[3]
--sumAcc ((0+1)+2) [3]
--sumAcc ((0+1)+2) 3:[]
--sumAcc ((0+1)+2)+3 []
--((0+1)+2)+3
-- (1+2)+3
-- +3
--6

--After applying bang pattern
--sum' [1,2,3]
--sumAcc 0 [1..3]
--sumAcc 0 (1:[2..3])
--sumACC (0+1) [2,3]
--sumACC 1 2:[3]
--sumAcc (1+2) [3]
--sumAcc 3 3:[]
--sumAcc (3+3) []
--sumAcc 6 []
--6

--length using acc  

lengthAcc :: Int -> [a] -> Int
lengthAcc !acc [] = acc
lengthAcc !acc (_:xs)  = lengthAcc (acc +1) xs

length :: [Int] -> Int
length = lengthAcc 0



