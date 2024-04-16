main :: IO ()
main = do
    print "Hello NammaYatri"
    print (succ 8)
    print (succ 9 + max 5 4 + 1)
    print (succ (9 * 10))
    print (doubleMe 4)
    print (doubleUs 2 3)
    print (isAdult 17)
    print (take 2 [1,4,5,6])
    print (min 7 8)
    
    print (filter odd [1,2,3,4,5])
    print (sum [1,2,3,4,5])
    --Anonymous Functions
    print ((\x -> x + 3) 10)
    print ((\ list n -> take n (reverse list)) [1,2,3,4,5,6,7] 3)

doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
isAdult age = if age >= 18 then "yes" else "No"

