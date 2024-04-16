main = do
    print (fTypeInt 30 10)
    --int getting overFLowed
    -- print( fTypeInt 223232324324234234234234 2421452152353252352532)
    --Integer not getting overflowed
    print( fTypeInteger 223232324324234234234234 2421452152353252352532)
    --float less precision
    print( fTypeFloat 2.0302033 3.3232424)
    --Double more precision
    print( fTypeDouble 2.0302033 3.3232424)
    

--int
---type decalaration
fTypeInt :: Int -> Int -> Int
fTypeInt x y =  x*x + y*y

--Integer

fTypeInteger :: Integer -> Integer -> Integer
fTypeInteger x y = x*x + y*y

--Float

fTypeFloat :: Float -> Float -> Float
fTypeFloat x y = x*x + y*y

--Double

fTypeDouble :: Double -> Double -> Double
fTypeDouble x y = x*x + y*y

--Bool




