removeLongWords :: Int -> String -> String
removeLongWords n s = helper [] n s
    where
        helper acc n "" = unwords acc
        helper acc n (unwords (x : (words s))) =  if (length x > n) then helper (x:acc) n s