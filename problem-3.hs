--problem-3
--What is the largest prime factor of the number 600851475143 ?

main :: IO()
main = print $

factors :: Integer -> [Integer]
factors n = [ x | x <- [2 .. (ceiling . sqrt $ fromIntegral n) ], n `mod` x == 0 ]

prime :: Integer -> Boolean
prime n = length . factors n == 0