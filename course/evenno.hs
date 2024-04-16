evens :: [Int] -> [Int]
evens [] = []
-- evens (x : a) = if even x then x : evens a else evens a
evens (x : a) = if x `mod` 2 == 0 then x : evens a
else evens a