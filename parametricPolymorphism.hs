main = do
    print( take 2 [1,2,3,4,5])
    print(3 +$+ 4)
    print( 1:[1,2,3,4])


--symbolic function
(+$+) :: Int -> Int -> Int
x +$+ y = x + y
