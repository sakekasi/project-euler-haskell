--Find the sum of all the primes below two million.
import Data.Numbers.Primes

main :: IO()
main = print $ foldr (+) 0 $ primes_below 2000000

primes_below :: Integer -> [Integer]
primes_below n = takeWhile (< n) primes