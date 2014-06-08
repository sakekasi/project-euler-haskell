--Find the maximum total from top to bottom of the triangle below
import Data.Array
import System.Environment (getArgs)
import Control.Applicative ((<$>))

main :: IO()
main = do 
  path <- head <$> getArgs
  triangle <- read_triangle path
  putStrLn $ show $ max_path triangle (tri_size triangle) 1 

tri_size :: Array Int (Array Int Int) -> Int
tri_size triangle = case bounds triangle of
    (s, f) -> (f - s) +  1

--the triangle in this case is a 2d array
--(similar to how pascal's triangle is mathematically described)
--read the tree
read_triangle :: FilePath -> IO (Array Int (Array Int Int))
read_triangle fp = (tri . reverse . lines) <$> readFile fp
  where
    tri lines = listArray (1, length lines) (map row lines)
    row line = listArray (1, length $ nums line) (nums line)
    nums = map (read :: String -> Int) . words
  
--OPT(r,i) = v_ri + max(OPT(r-1,i), OPT(r-1,i+1))
--OPT(N,i) = v_ri
max_path :: Array Int (Array Int Int) -> Int -> Int -> Int
max_path triangle row index = (opt!row)!index
  where opt = listArray (1,row) $ (triangle!1 : map f [2..row])
        f r = listArray (1,rsize r) $ map (g r) [1..(rsize r)]
        g r i = triangle!r!i + max (opt!(r-1)!i) (opt!(r-1)!(i+1))
        rsize r = tsize - r + 1
        tsize = tri_size triangle
