import Data.Char (isNumber)
import Data.List (groupBy, transpose)
import Data.Bifunctor (second)
import Control.Monad.State (State, evalState, gets, get, modify)

main :: IO ()
main =  readFile ".\\input.txt" >>= print . solve2 . toInput . lines

type Board = [[(Bool, Int)]]

toInput :: [String] -> ([Int], [Board])
toInput []     = ([], [])
toInput (l:ls) = (toInts l, toBoards ls)
 where
  toInts :: String -> [Int]
  toInts =  map read . filter (/= ",") . groupBy (\x y -> isNumber x && isNumber y)

  toBoards :: [String] -> [Board]
  toBoards [] = []
  toBoards xs = (map (zip (repeat False) . map read . words) . drop 1 . take 6) xs : toBoards (drop 6 xs)

-- | Solution for part 1
solve1 :: ([Int], [Board]) -> Int
solve1 (i, bs) = uncurry (*) . second computeScore $ evalState (solveS i) bs

-- | The game for part 1
solveS :: [Int] -> State [Board] (Int, Board)
solveS []     = error "Somebody always wins"
solveS (x:xs) = do
    modify (map (updateBoard x))
    b <- gets (filter hasWon)
    if null b then solveS xs
              else return (x, head b)

-- | Solution for part 2
solve2 :: ([Int], [Board]) -> Int
solve2 (i, bs) = uncurry (*) . second computeScore $ evalState (solve2S i) bs
 where
  -- | The game for part 2. It uses game 1 as a subgame
  solve2S :: [Int] -> State [Board] (Int, Board)
  solve2S []     = error "Somebody always wins"
  solve2S (x:xs) = do
      b <- get
      if length b /= 1 then modify (filter (not . hasWon) . map (updateBoard x)) >> solve2S xs
                       else solveS (x:xs)

hasWon :: Board -> Bool
hasWon b = let markings = map (map fst) b in any and markings || any and (transpose markings)

updateBoard :: Int -> Board -> Board
updateBoard x = map (map (\(b, y) -> if y == x then (True, y) else (b, y)))

computeScore :: Board -> Int
computeScore = sum . map snd . concatMap (filter (not . fst))