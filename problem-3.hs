--problem-3
--What is the largest prime factor of the number 600851475143 ?

main :: IO()
main = print $ foldr (\x y -> if x > y then x else y) 0 $ filter prime $ factors 600851475143

factors :: Integer -> [Integer]
factors n = do
	let a = [ x | x <- [2 .. (ceiling . sqrt $ fromIntegral n) ], n `mod` x == 0 ]
	let b = [ n `div` x | x <- a]
	b ++ a

prime :: Integer -> Bool
prime n = length (factors n) == 0