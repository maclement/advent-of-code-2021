import Data.Char (isAlpha)
import Data.List (group, sort, nub, sortOn, groupBy)
import Data.Bool (bool)
import Data.Bifunctor (first)
import Data.Function (on)

main :: IO ()
main =  readFile ".\\input.txt" >>= print . solve 40 . toInput . lines

example :: (Map, Rule)
example = toInput ["NNCB","","CH -> B","HH -> N","CB -> H","NH -> C","HB -> C","HC -> B","HN -> C","NN -> C"
                            ,"BH -> H","NC -> B","NB -> B","BN -> B","BB -> N","BC -> B","CC -> N","CN -> C"]

type Rule = [((Char, Char), Char)]

type Map = [((Char, Char), Int)]

type Count = [(Char, Int)]

toInput :: [String] -> (Map, Rule)
toInput [] = ([], [])
toInput (s:ss) = (toMap s, map toRule $ drop 1 ss)
 where
  toRule :: String -> ((Char, Char), Char)
  toRule (l1:l2:rhs) = (,) (l1,l2) (head $ dropWhile (not . isAlpha) rhs)
  toRule _           = error ""

  toMap :: [Char] -> Map
  toMap = map (flip (,) 1) . splitPair

solve :: Int -> (Map, Rule) -> Int
solve n (m, r) = let counts = map snd $ toCount $ iterate (updateMap r) m !! n
                 in maximum counts - minimum counts

toCount :: Map -> Count
toCount m = let chars  = nub . concatMap ((\(x,y) -> [x,y]) . fst ) $ filter ((/= 0) . snd) m
                -- groups and sorts a list of pairs and then sums over second components for each group
                fuse x = map (foldr1 (\(i, j) p2 -> (i, snd p2 + j))) . groupBy ((==) `on` fst) . sortOn fst $ x
                -- counts how many times a char appears on the right/left side of a pair
                lefts  = fuse $ map (first fst) m
                rights = fuse $ map (first snd) m  
            in map (combine lefts rights) chars

combine :: Count -> Count -> Char -> (Char, Int) 
combine l r c = case (lookup c l, lookup c r) of
  (Nothing, Just x) -> (c, x)
  (Just x, Nothing) -> (c, x)
  (Just x, Just y)  -> (c, max x y)
  _                 -> (c, 0)

splitPair :: [a] -> [(a,a)]
splitPair (x:y:xs) = (x, y) : splitPair (y:xs)
splitPair _ = []

incrementOrInsert :: (Char, Char) -> Int -> Map -> Map
incrementOrInsert p i []             = [(p, i)]
incrementOrInsert p i (q@(p2, i2):m) | p == p2 = (p2, i2 + i) : m
                                     | otherwise = q : incrementOrInsert p i m

updateMap :: Rule -> Map -> Map
updateMap r [] = []
updateMap r ((p@(x, y), i) : xs) 
  | i <= 0    = (p, i) : updateMap r xs
  | otherwise = 
    case lookup p r of
      Nothing -> (p, i) : updateMap r xs
      Just c -> incrementOrInsert (x,c) i $ incrementOrInsert (c,y) i $ updateMap r xs