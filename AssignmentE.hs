---------------------------------------------------------------------
-- E.hs
--
-- (c) 2017-2021 Andres Loeh, Well-Typed LLP

{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}
module E where

import Prelude hiding (readLn)
import Control.Monad (liftM)
import Data.Char
import System.Directory
import System.IO
import System.IO.Error
import Distribution.Compat.Prelude (readMaybe, foldl1)
import Distribution.Simple.Build (repl)
import System.Random



-- import System.Random

-- Task E-1.
--
-- The function 'read' is the counterpart of 'show'.
-- Its type is
--
--   read :: Read a => String -> a
--
-- It tries to parse the string as a type, depending on
-- context.
--
-- For example, the following function tries to
-- parse a string to an integer simply by restricting
-- the type signature of 'read'. Unfortunately, 'read'
-- will crash if we call it on a malformed string.
--
-- Try both a successful and an unsuccessful call to
-- 'unsafeReadInt' in GHCi.
--
-- PLEASE DESCRIBE HERE
-- what you've seen when trying successful and unsuccessful call.
-- on successfull call like unsafeReadInt "123" i got an Int 
-- on an unsuccesfull call eg unsafeReadInt "sfr" i got Exception of no parse

unsafeReadInt :: String -> Int
unsafeReadInt = read

-- Task E-2.
--
-- The function 'readMaybe', defined in the module
-- 'Text.Read', fixes this problem by returning a
-- 'Maybe' result instead. Define a safe version of
-- 'readInt' by using it.

readInt :: String -> Maybe Int
readInt str = readMaybe str :: Maybe Int

-- readInt = error "TODO: implement readInt"

-- Task E-3.
--
-- Implement the IO action 'readLnMaybe' that reads a
-- line from the user, and tries to convert it into
-- a value of another readable type via 'readMaybe'.
--
-- Hint: Use 'fmap'.

readLnMaybe :: Read a => IO (Maybe a)
readLnMaybe = fmap readMaybe getLine
-- readLnMaybe = error "TODO: implement readLnMaybe"

-- Task E-4.
--
-- Define a function that prints a given string and then
-- reads a line from the user, tries to convert it into
-- a value of type 'Int', and if that fails, asks again.
--
-- Example:
--
--   >>> askInt "Please enter a number:"
--   Please enter a number:
--   foo
--   Please enter a number:
--   1+2
--   Please enter a number:
--   33
--
--   The result in the above interaction would be 33,
--   because the first two attempts are no numbers.

askInt :: String -> IO Int
askInt str = do
    putStrLn str
    x <- readLnMaybe :: IO(Maybe Int)
    case x of
        Just a -> return a
        Nothing -> askInt str

-- askInt = error "TODO: implement askInt"

-- Task E-5.
--
-- Write a program that asks the user to input a number, and after that
-- number has been provided, a second number, and then replies with the
-- sum of both numbers.
--
-- Example:
--
--   Please enter first number:
--   12
--   Please enter second number:
--   23
--   The sum of both numbers is 35.
--
-- Use 'askInt' for reading the inputs. Remember that
--
--   show :: Show a => a -> String
--
-- can be used to convert a number back into a string. Also
-- remember that strings are lists of characters.

sumTwo :: IO ()
sumTwo = do
    n1 <- askInt  "Please enter first number:"  -- retrive Int value from IO Int
    n2 <- askInt  "Please enter second number:"
    print (n1 + n2)



-- sumTwo = error "TODO: define sumTwo"

-- Task E-6.
--
-- Define a function that, given a number n and an IO action, constructs
-- an IO new action that executes the original action n times, collecting
-- the results in a list of n elements.

replicateM :: Int -> IO a -> IO [a]
replicateM 0 _ = return []
replicateM n a = do
    res <- a
    rem <- replicateM (n-1) a
    return (res : rem)

-- replicateM = error "TODO: define replicateM"

-- Task E-7.
--
-- Define a program that first asks the user to provide the number of
-- items to add. Then, if the user enters N, the program should ask for
-- N numbers. Once these have been entered, the program should return
-- the result.
--
-- Example:
--
--   How many numbers do you want to add?
--   3
--   Enter next number:
--   10
--   Enter next number:
--   1
--   Enter next number:
--   7
--   The sum of all numbers is 18.
--
-- Hint: Try to use suitable IO functions.

sumMany :: IO ()
sumMany = do
    n <- askInt "How many numbers do you want to add?"
    nums <- replicateM n (askInt "Enter next number :")
    let total = sum nums
    print (total)

-- sumMany = error "TODO: define sumMany"

-- Task E-8.
--
-- Change 'sumMany' such that when asking for each number,
-- an indication of progress is given.
--
--   How many numbers do you want to add?
--   3
--   Enter number 1 of 3:
--   10
--   Enter number 2 of 3:
--   1
--   Enter number 3 of 3:
--   7
--   The sum of all numbers is 18.
--
-- Hint: You cannot use 'replicateM' for this. But perhaps
-- you can still find higher-order functions that work?

sumMany' :: IO ()
sumMany' = do
     n <- askInt "How many numbers do you want to add?"
     nums <- sequence $ map (\i -> askInt $ "Enter number " ++ show i ++ " of " ++ show n ++ ":") [1..n]
     let total = sum nums
     print (total)
-- sumMany' = error "TODO: define sumMany"

-- Task E-9.
--
-- Define a function 'compareFileSizes' that takes two file paths.
-- It tries to determine the size of the two files compares them.
-- If successful, it will return a result of type 'Ordering'
-- describing the relationship between the two file sizes. If
-- accessing any of the files fails, then 'Nothing' is returned
-- instead.
--
-- Note that you will have to catch 'IOError' exceptions in order
-- to get this behaviour implemented correctly.
--
compareFileSizes :: FilePath -> FilePath -> IO (Maybe Ordering)
compareFileSizes f1 f2 = do
    s1 <- getFileSize f1
    s2 <- getFileSize f2
    return $ case (s1,s2) of
        (a ,b) -> Just (compare a b )
        _     -> Nothing
-- compareFileSizes = error "TODO: define compareFileSizes"

-- Task E-10.
--
-- Define an IO action that "rolls two (6-sided) dice" and returns the
-- two results as a pair.
--
-- Use the function
--
--   randomRIO :: Random a => (a, a) -> IO a
--
-- from the module System.Random (which you will have to add to the
-- import list).
--
-- It takes a range as an input, i.e., the minimal and maximal value
-- that the random number generator should produce. The 'Int' type is
-- an instance of the 'Random' class.
--
-- Note: you need to provide correct type signature yourself
--
roll :: IO (Int,Int)
roll = do
    d1 <- randomRIO (1,6)
    d2 <- randomRIO (1,6)
    return (d1,d2)
-- roll = error "TODO: define roll"

-- Task E-11.
--
-- The following datatype defines the abstract syntax of a language
-- of dice expressions.
--
-- For example, in an imagined concrete syntax, the string
--
--   2d8 + 4
--
-- stands for "roll two 8-sided dice and add 4". The abstract
-- representation of this expression is
--
--   2 `D` 8 `Plus` Const 4
--
-- Define an evaluator 'dice' that uses a random number generator
-- to produce a result.

data Dice =
    D Quantity Die
  | Const Int
  | Plus Dice Dice
  deriving (Show, Eq)

infix  7 `D`
infixl 6 `Plus`

type Quantity = Int
type Die      = Int

dice :: Dice -> IO Int
dice (Const a)    = return a
dice (D q d)      = sum' q d
dice (Plus a b)   = do
    res1 <- dice a
    res2 <- dice b
    return (res1 + res2)

-- dice D q d   = 
-- dice = error "TODO: implement dice"
sum' :: Quantity -> Die -> IO Int
sum' 0 _ = return 0
sum' n d = do
    roll <- randomRIO (1,d)
    next <- sum' (n-1) d
    return (roll + next)


-- Task E-12.
--
-- Define another function on the 'Dice' type called 'diceRange'
-- that returns the minimal and maximal random value that could
-- be produced by the given expression.
--
-- Example:
--
--   diceRange (2 `D` 8 `Plus` Const 4) == (6, 20)

diceRange :: Dice -> (Int, Int)
diceRange (Const a)    = (a, a)
diceRange (D q d)      = (q, q * d) -- returns (min,max)
diceRange (Plus a b)   = let (min1, max1) = diceRange a
                             (min2, max2) = diceRange b
                         in (min1 + min2, max1 + max2)



-- diceRange = error "TODO: implement diceRange"

-- Task E-13.
--
-- Shuffle a deck of cards (or any other list).
--
-- Here's a standard naive algorithm:
--
-- To shuffle a list, pick a random number that is a valid index into the
-- list. Remove the element from the list and make it the first element of
-- the shuffled list. Then recurse on the shorter list. Once the empty list
-- is reached, stop.
--
-- You may need to write a function
--
--   extract :: Int -> [a] -> (a, [a]) -- possibly crashing
--
-- that finds the element contained in a list at a particular index
-- and returns it together with the rest of the list.
-- (A better data structure for this kind of operation would be a sequence.)
--
-- The following data types are just given for flavour:

data Suit = Clubs | Spades | Hearts | Diamonds
  deriving (Show, Eq, Bounded, Enum)

data CardNumber = A | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10 | J | Q | K
  deriving (Show, Eq, Bounded, Enum)

data Card = Card CardNumber Suit
  deriving (Show, Eq)

-- This list comprehension syntax offered by Haskell is just syntactic
-- sugar for applications of the functions 'map' and 'concat'. Do you
-- understand what it does?

allCards :: [Card]
allCards = [ Card cn s | cn <- [minBound ..], s <- [minBound ..] ]



extract :: Int -> [a] -> (a, [a])
extract _ []     = error "Index out of bounds"
extract 0 (x:xs) = (x, xs)
extract n (x:xs) = let (elem, rest) = extract (n - 1) xs
                   in (elem, x : rest)

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
    index <- randomRIO (0, length xs - 1)
    let (elem, rest) = extract index xs
    shuffledRest <- shuffle rest
    return (elem : shuffledRest)