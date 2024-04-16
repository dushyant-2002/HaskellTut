import System.IO
-- import System.Random (randomIO)

data Interaction =
     Question String Interaction Interaction
    | Result String
    deriving Show

example :: Interaction
example =
    Question "Do you like static types?"
        (Question "Do you like linear or affine types?"
            (Result "Try Rust!")
            (Question "Do you like dependent types?"
                (Result "Try Idris!")
                (Result "Try OCaml!")
            )
        )
        (Question "Do you like parentheses?"
            (Result "Try Clojure!")
            (Result "Try Erlang!")
        )

askYesNo :: String -> IO Bool
askYesNo q = do
    putStrLn (q ++ " [yn]")
    c <- getChar
    hFlush stdout -- flushes the buffer immmediately
    putStrLn "  " -- newline for better formatting
    case c of
        'y' -> return True
        'n' -> return False
        _   -> do
            putStrLn "Invalid input, please enter 'y' or 'n'."
            askYesNo q

runInteraction :: Interaction -> IO ()
runInteraction (Question q y n) = do
    b <- askYesNo q
    --case b of
    --     True -> runInteraction y
    --     False-> runInteraction n
    if b
        then runInteraction y
        else runInteraction n
runInteraction (Result r) = putStrLn r

--simulate with list of answers
simulate :: Interaction -> [Bool] -> Maybe String
simulate (Question _ y _) (True : as) = simulate y as
simulate (Question _ _ n) (False : as) = simulate n as
simulate (Result r)         _           = Just r
simulate _                  _           = Nothing
    

--Self Task
--longest Path

longestPath :: Interaction -> Int
longestPath (Result r) = 0
longestPath (Question _ y n) = 1 + max (longestPath y) (longestPath n)

---InteractRandomly
-- interactRandomly :: Interaction -> IO String
-- interactRandomly (Result r) = r
-- interactRandomly (Question q y n) = do
--     choice <- randomIO :: Bool
--     case choice of
--         True -> interactRandomly y
--         False -> interactRandomly n
