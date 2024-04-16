{-# LANGUAGE  InstanceSigs #-}

-- import Prelude hiding (Functor(..))


mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList f (x:xs) = f x : mapList f xs


mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing = Nothing
mapMaybe f (Just x) = Just (f x)




mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Node l r) = Node (mapTree f l) (mapTree f r)

class MyFunctor f where
    myfmap :: (a -> b) -> f a -> f b

instance MyFunctor [] where 
    myfmap :: (a -> b) -> [a] -> [b]
    myfmap = mapList

instance MyFunctor Maybe where
    myfmap :: (a -> b) -> Maybe a -> Maybe b
    myfmap = mapMaybe

instance MyFunctor Tree where
    myfmap :: (a -> b) -> Tree a -> Tree b
    myfmap = mapTree

instance MyFunctor (Either x) where
    myfmap :: (a -> b) -> Either x a -> Either x b
    myfmap _ (Left x) = Left x
    myfmap f (Right a) = Right (f a)

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show

--make folable instance for tree
toList :: Tree a -> [a]
toList (Leaf x) = [x]
toList (Node l r) = toList l ++ toList r


instance Foldable Tree where
    foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr cons nil t = foldr cons nil (toList t)
newtype Matrix a = Matrix [[a]]
  deriving (Show, Functor, Foldable)


  