 -- problem-1
 -- Find the sum of all the multiples of 3 or 5 below 1000.


 main :: IO()
 main = print $ foldr (\x y -> x + y) 0 [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]