import Prelude hiding (getLine)

getLine :: String
getLine = error "Some magic happens"

--program 1
--Read a line and then read a second line and return both strings completed

program1 :: String
program1 = 
    let
        x = getLine
        y = getLine
    in
        x ++ y

--by equational reasoning
program1' :: String
program1' = getLine ++ getLine


--program 2
--Read a line and return the string appended to itself
program2 = 
    let
        x =getLine
    in
        x ++ x

program2' :: String
program2' = getLine ++ getLine


--program 3
--read a line and then read second line and return both strings appended in flipped order
program3 :: String
program3 = 
    let
        x = getLine
        y = getLine
    in
        x ++ y

program3' :: String
program3' = getLine ++ getLine

--no one have any difference coz we can't differentiate when we took input of x and y due to lazy evaluation
--to handle we use IO type in haskell IO is not of type String it is of Type IO

