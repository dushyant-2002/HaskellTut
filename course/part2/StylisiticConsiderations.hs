import Prelude hiding (product)
--Spacing Issues

product :: [Int] -> Int
product []          = 1
--remove redundant cases
-- product [e1]        = e1
-- product [e1,e2]     = e1 * e2
product (p : array) = p * product array