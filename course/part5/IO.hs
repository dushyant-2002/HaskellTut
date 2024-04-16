{-# OPTIONS_GHC -Wall #-}
import qualified Prelude hiding (sequence)
import Prelude hiding (sequence, (>>))
import Data.Char

import qualified Control.Applicative

import Data.Time.Format.ISO8601 (yearFormat)
import Control.Monad.RWS (MonadState(put))

main :: IO ()
main = putStrLn "fsdfsf"

--sequencing

(>>) :: IO a -> IO b -> IO b 
-- (>>) = (Prelude.>>) --to cha nge default type signatur from monad to IO a -> IO b -> IO b
(>>) = liftA2 (\_ b -> b) --in Terms of liftA2

--liftA2
liftA2 :: (a -> b -> c) -> IO a -> IO b -> IO c
liftA2 = Control.Applicative.liftA2



--program 1 : read two lines and append its result
program1 :: IO String
program1 = liftA2 (++) getLine getLine

--program2 :: read one line and append result to its own
program2 :: IO String
-- program2 = 
--     let 
--         x = getLine
--     in
--         fmap (\y -> y ++ y) x
program2 = fmap (\x -> x ++ x) getLine


--program 3 :: read two lines and append them after flipping
program3 :: IO String
program3 = liftA2 (\x y -> y ++ x) getLine getLine

echo :: IO ()
echo = getLine >>= putStrLn

-- (>>=) :: IO a -> (a -> IO b) -> IO b
-- (>>=) getLine :: (String -> IO b) -> IO b
-- (>>=) putStrLn :: IO ()

echoReverse :: IO ()
-- echoReverse = getLine >>= (\x -> putStrLn (reverse x))
echoReverse = do
    x <- getLine
    putStrLn (reverse x)

echoTwice :: IO ()
-- echoTwice = getLine >>= (\x -> putStrLn x >> putStrLn x)
echoTwice = do
    x <- getLine
    putStrLn x
    putStrLn x


-- print back input in uppercase letters
shoutBackTwice :: IO ()
-- shoutBackTwice = getLine >>= \x -> 
--     let y = map toUpper x 
--     in putStrLn y >> putStrLn y
shoutBackTwice = do
    x <- getLine
    y <- return (map toUpper x)
    putStrLn y
    putStrLn y


combineLines :: IO ()
-- combineLines = 
--     getLine >>= \x ->
--     getLine >>= \y ->
--     putStrLn (x ++ "," ++ y)
--using do
combineLines = do
    x <- getLine
    y <- getLine
    putStrLn ( x++ "," ++ y)

--Using liftA2
combineLines' :: IO ()
combineLines' = 
    liftA2 (\x y -> x ++ "," ++ y) getLine getLine >>= \c ->
    putStrLn c

--fmap in IO
fmap' :: (a -> b) -> IO a -> IO b
fmap' f ioa = 
    ioa >>= (\a -> return (f a))
    
-- creating liftA2
liftA2' :: (a ->b -> c) -> IO a -> IO b -> IO c
liftA2' f ioa iob = 
    ioa >>= \a ->
    iob >>= \b ->
    return (f a b)

--liftA2' using do
-- liftA2' f ioa iob = do
--     a <- ioa
--     b <- iob
--     return (f a b)



sizeOfFile :: FilePath -> IO ()
sizeOfFile filePath=
    let 
        x = readFile filePath
    in
        x >>= \a -> print (length a)
    

--Do notation

conversation :: IO ()
conversation = 
    -- putStrLn "Please provide some input" >> getLine >>= \x ->
    ask "Please provide some input" >>= \x ->   --same as previous line
    if null x
        then putStrLn "Bye!"
        else putStrLn ("Thanks for the input (" ++ x ++ "). ") >> conversation


ask :: String -> IO String
ask prompt = 
    putStrLn  prompt >>
    getLine


--conversation using do

conversation' :: IO ()
conversation'= do
    x <- ask "Please provide some input"
    if null x
        then putStrLn "Bye!"
        else do 
            putStrLn ("Thanks for the input (" ++ x ++ "). ") 
            conversation

--liftA2 using do
liftA2'' :: (a ->b -> c) -> IO a -> IO b -> IO c
liftA2'' f ioa iob = do
    a <- ioa
    b <- iob
    return (f a b)


--ShoutBacktwice using do

shoutBackTwice' :: IO ()
shoutBackTwice' = do
    x <- getLine
    let y = map toUpper x
    putStrLn y 

-- ask using do

ask' :: String -> IO String
ask' prompt = do
    putStrLn prompt
    getLine
     

example1 :: IO ()
example1 = do
  putStrLn "!"

example2 :: IO String
example2 = do
  _ <- getLine
  y <- getLine
  return y



--askMany using do

askMany :: [String] -> IO [String]
askMany []          = return []
-- askMany (x : xs)    = do
--     a <- ask x
--     as <- askMany xs
--     return (a : as)
askMany (x : xs) = liftA2 (:) (ask x) (askMany xs)


--sequence using do
sequence :: [IO a] -> IO [a]
sequence [] = return []
-- sequence (x :xs)  = do
--     a <- x
--     as <- sequence xs
--     return (a : as)
--using liftA2
sequence (x : xs) = liftA2 (:) x (sequence xs)


askMany'' :: [String] -> IO [String]
askMany'' = sequence . map ask

--filter  

filterM :: (a -> IO Bool) -> [a] -> IO [a]
filterM p [] = return []
filterM p (x:xs) = do
    b <- p x
    -- if b
    --     then do
    --         ys <- filterM p xs
    --         return (x : ys)
    --     else filterM p xs
    if b
        -- then fmap (\ys -> x : ys) (filterM p xs)  -- fmap (x :) (filterM p xs) -- (x:) <$> (filterM p xs)
        then (x :) <$> filterM p xs
        else filterM p xs

askNull :: String -> IO Bool
askNull q = do
    a <- ask q
    return (null a) 


    