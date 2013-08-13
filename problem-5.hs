--problem-5.hs
--What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

main :: IO()
main = print $ head $ dropWhile (\x -> not $ foldr (&&) True $ map (divisible x) [1 .. 20]) [1 ..]

divisible :: Integer -> Integer -> Bool
divisible x y = mod x y == 0