import Prelude hiding (or, reverse, filter, replicate, repeat)



-- B 1

-- data Either a b = Left a | Right b

collapse :: Either [Int] Int -> Int
collapse (Left a)   = sum a
collapse (Right b)  = b



-- B - 2
newtype Month = MkMonth Int
  deriving Show

mkMonth :: Int -> Maybe Month
mkMonth a | (a >0 && a <= 12)   = (Just (MkMonth a))
          |otherwise            = Nothing


--B-3

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _  Nothing     = Nothing
mapMaybe f  (Just a)    = Just (f a)

-- B4

pairMaybe :: Maybe a -> Maybe b -> Maybe (a,b)
pairMaybe (Just a) (Just b) = Just (a,b)
pairMaybe    _        _     = Nothing

-- B5

liftMaybe :: (a->b->c) -> Maybe a ->  Maybe b -> Maybe c
liftMaybe f (Just a) (Just b)   = Just (f a b)
liftMaybe _ _ _                 = Nothing


-- B6

pairMaybe' :: Maybe a -> Maybe b -> Maybe (a,b)
pairMaybe' a b = liftMaybe (\a b-> (a,b)) a b


--B7 --left
table = [(1, "x"), (2, "y"), (3, "z")]
lookups :: Eq a => [a] -> [(a, b)] -> Maybe [b]
lookups [] _ = Just []  -- Base case: an empty list always succeeds
lookups (k:ks) table
    | Just v <- lookup k table, Just vs <- lookups ks table = Just (v : vs)
    | otherwise = Nothing


-- B 8

reverse :: [a] -> [a]
reverse []          = []
reverse (x : xs)    = reverse xs ++ (x:[])


-- B 9

reverseAcc :: [a] -> [a] -> [a]
reverseAcc xs []        = xs
reverseAcc xs (y : ys)  = reverseAcc (y:xs) ys


-- B 10
-- Yes reverseAcc is faster because it is not using any operator like ++ so it takes less time than reverse
-- ++ implementation looks like that
--(++) :: [a] -> [a] -> [a]
-- [] ++ [] = []
-- [] ++ (y:ys) = y : ys
-- (x:xs) ++ ys = x : (xs ++ ys)


-- B 11

replicate :: Int -> a -> [a]
replicate 0 a = []
replicate n a | n > 0     = a : replicate (n-1) a
              | otherwise = []


--B12

repeat :: a -> [a]
repeat a = a : repeat a

--B13

replicate' :: Int -> a -> [a]
replicate' 0 a = []
replicate' n a = take n (repeat a)


--B14
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Eq)
tree1 :: Tree Int
tree1 = Leaf 1

tree2 :: Tree Int
tree2 = Node (Leaf 2) (Leaf 4)

tree3 :: Tree Int
tree3 = Node tree2 tree1

tree4 :: Tree Int
tree4 = Node tree2 tree3

tree5 :: Tree Int
tree5 = Node (Leaf 3) (Leaf 3)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf a) = Leaf (f a)
mapTree f (Node a b) = Node (mapTree f a) (mapTree f b)

--B 15

sameShape :: Tree a -> Tree b -> Bool
sameShape (Leaf a) (Leaf b) = True
sameShape (Node a1 b1) (Node a2 b2) = (sameShape a1 a2) && (sameShape b1 b2)
sameShape _ _ = False


--B 16

-- sameShape' :: Tree a -> Tree b -> Bool

--B 17

buildTree :: Int -> Tree()

buildTree 0 = Leaf()
buildTree 1 = Node (Leaf ()) (Leaf ())
buildTree n | n > 0 = Node (buildTree (n-1)) (buildTree (n-1))
            | otherwise = Leaf ()

-- B 18

data Expr =
    Lit Int
  | Add Expr Expr
  | Neg Expr
  | IfZero Expr Expr Expr
  deriving (Eq, Show)

expr1 :: Expr
expr1 = Neg (Add (Lit 3) (Lit 5))

expr2 :: Expr
expr2 = IfZero expr1 (Lit 1) (Lit 0)

eval :: Expr -> Int
eval (Lit a) = a 
eval (Neg a) = (-(eval a))
eval (Add a b) = sum [eval a ,eval b]
eval (IfZero a b c) | eval a == 0 = eval b
                    | otherwise = eval c


prop_eval1 :: Bool
prop_eval1 = eval expr1 == -8

prop_eval2 :: Bool
prop_eval2 = eval expr2 == 0

-- B 19







