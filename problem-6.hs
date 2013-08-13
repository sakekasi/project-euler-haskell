--problem-6.hs
--Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

main :: IO()
main = print $ abs $ sum_sqr 100 - sqr_sum 100

sum_sqr :: Integer -> Integer
sum_sqr n = foldr (+) 0 [x*x | x <- [1 .. n]]

sqr_sum :: Integer -> Integer
sqr_sum n = x * x
	where x = foldr (+) 0 [1 .. n]