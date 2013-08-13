--problem-2.hs
--By considering the terms in the Fibonacci sequence whose values do not 
--exceed four million, find the sum of the even-valued terms.


main :: IO()
main = print $ foldr (+) 0 even_fibs

fibs = map fst $ iterate (\(a,b) -> (b,a+b)) (0,1)
even_fibs = filter (\x -> even x) $ takeWhile (\x -> x < 4000000) fibs