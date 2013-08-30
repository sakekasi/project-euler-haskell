--There exists exactly one Pythagorean triplet for which a + b + c = 1000.
--Find the product abc.

main :: IO()
main = print $ tuple_product special_pythagorean

pythagorean :: (Int,Int,Int) -> Bool
pythagorean (a,b,c) = a*a + b*b == c*c

special_pythagorean :: (Int, Int, Int)
special_pythagorean = head $ [ (a,b,1000-a-b) | a <- [1 .. 998], b <-[1 .. 999-a], pythagorean (a,b,1000-a-b) ]

tuple_product :: (Int, Int, Int) -> Int
tuple_product (a,b,c) = a * b * c