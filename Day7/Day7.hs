import Data.Char ( isNumber )
import Data.List ( groupBy, permutations, sort) 

main :: IO ()
main =  readFile ".\\input.txt" >>= print . solve2 . toInput . lines

toInput :: [String] -> [Int]
toInput = concatMap $ map read . filter (all isNumber) . filter (/= ",") . groupBy (\x y -> isNumber x && isNumber y)

solve :: [Int] -> Int
solve = dist  . sort
 where
  dist :: [Int] -> Int
  dist xs = let m1 = (xs !! ((length xs -1) `div` 2))
                m2 = (xs !! (length xs `div` 2))
                f m = sum . map (abs . subtract m)
            in min (f m1 xs) (f m2 xs)

example :: [Int]
example = [16,1,2,0,4,2,7,1,2,14]

solve2 :: [Int] -> Int 
solve2 s = minimum $ zipWith (\i -> sum . map (gaus i)) [0 ..] (replicate (maximum s + 1) s)
 where
  gaus from to = let n = abs (from - to) in n * (n + 1) `div` 2