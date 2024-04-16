
data Choice = Rock | Paper | Scissors
    deriving (Eq,Ord,Show,Read,Enum,Bounded)

-- acc to > Rock < Paper < Scissors its acc to placement of constructor in data type


data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show,Eq,Ord,Read)
    