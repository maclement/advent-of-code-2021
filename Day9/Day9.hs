import Data.Bool ( bool )
import Data.Bifunctor ( bimap )
import Data.Either (isLeft, isRight)
import Data.List (minimumBy, group, sort)

main :: IO ()
main =  readFile ".\\input.txt" >>= print . solve2 . toInput . lines

toInput :: [String] -> [[Either Int Int]]
toInput = map (map (Left . read . return))

example :: [String]
example = ["2199943210","3987894921","9856789892", "8767896789", "9899965678"]

width :: [[a]] -> Int
width = length . head

height :: [[a]] -> Int
height = length

-- adds a 'Left 9' border to the list
addBorder :: [[Either Int Int]] -> [[Either Int Int]]
addBorder xs = let r = replicate (width xs + 2) (Left 9)
               in snoc r $ r : map (snoc (Left 9) . (Left 9:)) xs
 where
  snoc ::  a -> [a] -> [a]
  snoc x xs = xs ++ [x]


-- | Solution for task 1
solve1 :: [[Either Int Int]] -> Int
solve1 = sum . map ((+1) . fromEither) . filter isRight . concat . sinks

-- | Computes all sinks and flips them from 'Left' to 'Right'
sinks :: [[Either Int Int]] -> [[Either Int Int]]
sinks input = let borderedInput = addBorder input
                  isSink = \(x,y) -> bool (Left x) (Right x) (all (x<) y)
              in map (isSink . bimap fromEither (map fromEither) . adjacent borderedInput) <$> positionGrid input

fromEither :: Either Int Int -> Int
fromEither = either id id

-- | All adjacent values to a value at position i j
adjacent :: [[a]] -> (Int, Int) -> (a, [a])
adjacent m (i, j) = (m !! i !! j, [m !! pred i !! j, m !! succ i !! j, m !! i !! pred j, m !! i !! succ j])

-- | All save positions of a bordered list
positionGrid :: [[a]] -> [[(Int, Int)]]
positionGrid xss = flip zip [1..width xss] <$> map (replicate (width xss)) [1..height xss]

-- | Find the sink to a given pair of indices
flow :: [[Either Int Int]] -> (Int, Int) -> (Int, Int)
flow xss (i, j) | fromEither (xss !! i !! j) == 9 = (i, j)
                | otherwise                       = either (const flowRec) (const (i,j)) (xss !! i !! j)
 where
  comp :: (Int, Int) -> (Int, Int) -> Ordering
  comp = \(x1,y1) (x2, y2) -> compare (fromEither $ xss !! x1 !! y1) (fromEither $ xss !! x2 !! y2)

  flowRec :: (Int, Int)
  flowRec = flow xss $ minimumBy comp [(pred i, j), (succ i, j), (i, pred j), (i, succ j)]

-- | Solution for task 2
solve2 :: [[Either Int Int]] -> Int
solve2 input = let sinkedInput = addBorder $ sinks input
                   flowedInput = map (flow sinkedInput) <$> positionGrid input
               in product . take 3 . reverse . sort . map length . group . sort . concat $ flowedInput