--problem-7.hs
--What is the 10 001st prime number?

main :: IO()
main = print $ nth_prime 10001

nth_prime :: Int -> Integer
nth_prime n = head $ drop (n-1) primes

primes:: [Integer]
primes = map head $ iterate (\(x:xs)-> filter ((> 0).(`rem`x)) xs) [2..]