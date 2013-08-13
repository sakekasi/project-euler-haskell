--problem-4.hs
--Find the largest palindrome made from the product of two 3-digit numbers.

main :: IO()
main = print $ foldr (\x y -> if x > y then x else y) 0 $ filter palindrome three_digit_products

three_digit_products=[x*y | x <- [100 .. 999], y <- [100 .. 999]]

palindrome :: Integer -> Bool
palindrome n = x == reverse x
	where x = digits n

digits :: Integer -> [Integer]
digits 0 = []
digits n = digits k ++ [r]
    where k = div n 10; r = mod n 10