
data Tree a =
    Leaf a
    | Node (Tree a) (Tree a)
    deriving (Show,Eq)

exampleTree :: Tree Int
exampleTree =
    Node (Leaf 2) (Leaf 3)

exampleTree2 :: Tree Int
exampleTree2 =
    Node
        (Node (Leaf 4) (Leaf 5))
        (Leaf 6)


data Date = Date Int Int Int
            deriving (Show,Eq)

date ::  Int -> Int -> Int -> Date
date a b c = Date a b c


data WeekDay = Mo | Tue | Wed | Thur | Fri | Sat | Sun

data User' = User' Int String String
        deriving Show

--can give 
-- data User = User { uid::Int,uname :: String, uemail:: String}
--         deriving Show

--by using own data type for uid to prevent operations on uid
data User = User { uid::UserId,uname :: Username, uemail:: Email}
        deriving Show

dushyant = User {uid = UserId 1 , uname = "dasd" , uemail = Email "sfs"}

data UserId = UserId Int
        deriving Show

uid1 :: UserId
uid1 = UserId 42

uid2 :: UserId
uid2 = UserId 22

data Email = Email {getEmail :: String}
    deriving Show

newtype Name = Name String
    deriving Show


type Username = String
type IntList = [Int]
x :: IntList
x = [1,2,3,4]





