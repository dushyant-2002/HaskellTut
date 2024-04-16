
--(>>=) :: IO a -> (a -> IO b) -> IO b
--liftA2 :: (a -> b -> c) -> IO a -> IO b -> IO c
-- fmap :: (a -> b) -> IO a -> IO b
import System.IO.Unsafe


getLine' :: String
getLine' = unsafePerformIO getLine