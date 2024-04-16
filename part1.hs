main = do
    print( take 2 [1,2,3,4,5])
    print(3 +$+ 4)
    print( 1:[1,2,3,4])
    print(map odd [1,2,3,4,5,6,7])
    --using () for changin infix to prefix
    print((:)1 [1,2,3,4])
    print((*) 7 9 )


--symbolic function
(+$+) :: Int -> Int -> Int
x +$+ y = x + y
