import Control.Monad.State ( modify, evalState, execState, State, foldM_, get, put)
import Data.Char ( isNumber )
import Data.List ( groupBy, transpose, group, sort)

main :: IO ()
main =  readFile ".\\input.txt" >>= print . solve . toInput . lines

data Line = (Int, Int) :->: (Int, Int)

toInput :: [String] -> [Line]
toInput =  map toLine
 where
  toLine :: String -> Line
  toLine s = let l@[x1, y1, x2, y2] = toInt s in (x1, y1) :->: (x2, y2)

toInt :: String -> [Int]
toInt = map read . filter (all isNumber) . filter (/= ",") . groupBy (\x y -> isNumber x && isNumber y)

toPoints :: Line -> [(Int, Int)]
toPoints ((x1, y1) :->: (x2, y2)) | x1 == x2 || y1 == y2 = [(p1, p2) | p1 <- [min x1 x2 .. max x1 x2], p2 <- [min y1 y2 .. max y1 y2]]
                                  | otherwise = zip (ini x1 x2) (ini y1 y2)
  where
   ini :: Int -> Int -> [Int]
   ini from to | from < to = [from .. to]
               | otherwise = reverse [to .. from]

solve :: [Line] -> Int
solve = length . filter (>= 2) . map length . group . sort . concatMap toPoints