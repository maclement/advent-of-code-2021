import Data.Bool  (bool)
import Data.List (partition, transpose)
import Data.Bifunctor (bimap)

main :: IO ()
main =  readFile ".\\input.txt" >>= print . solve2 . map toBin . lines

example :: [[Bool]]
example = map toBin ["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]

mult :: [Bool] -> Int
mult bs = toInt bs * toInt (map not bs)  

toBin :: String -> [Bool]
toBin = map toBit 
 where
  toBit :: Char -> Bool
  toBit '0' = False
  toBit '1' = True
  toBit _   = error ""

toInt :: [Bool] -> Int
toInt = foldl (\r b -> 2 * r + bool 0 1 b) 0

solve :: [[Bool]] -> Int
solve = mult . map majority . transpose

majority :: [Bool] -> Bool
majority = uncurry (>=) . bimap length length . partition id

solveGen :: ([Bool] -> Bool) -> [[Bool]] -> [Bool]
solveGen f [x] = x
solveGen f xss = let b = f (map head xss) in b : solveGen f (map tail (filter ((b ==) . head) xss))

solve2 :: [[Bool]] -> Int
solve2 bss = (toInt . solveGen majority) bss * (toInt . solveGen (not . majority)) bss