import Data.Char ( isAlpha, isUpper, toUpper, toLower )
import Data.Function ( on )
import Data.List ( groupBy, (\\) )
import Data.Bifunctor ( second, bimap )
import Data.Bool ( bool )
import Data.Tuple ( swap )

main :: IO ()
main =  readFile ".\\input.txt" >>= print . solve2 . toInput . lines

type Edge = (Cave String, Cave String )

type Path = [Cave String]

data Cave a = Small a | Large a
 deriving Eq

example :: [Edge]
example = toInput ["start-A","start-b","A-c","A-b","b-d","A-end","b-end"]

fromCave :: Cave a -> a
fromCave (Small x) = x
fromCave (Large x) = x

isSmall :: Cave a -> Bool
isSmall (Small _) = True
isSmall _         = False

toInput :: [String] -> [Edge]
toInput = concatMap toPair
 where
  toPair :: String -> [Edge]
  toPair s =  let p = bimap head (head . tail) . span (/= "-") . groupBy ((&&) `on` isAlpha ) $ s
                  isLarge x = bool (Small x) (Large x) (isUpper $ head x)
              in [bimap isLarge isLarge p, swap $ bimap isLarge isLarge p]

-- | Solution for part 1
solve1 :: [Edge] -> Int
solve1 es = length $ solve1' (Small "start") es [Small "start"]

solve1' :: Cave String -> [Edge] -> [Cave String] -> [Path]
solve1' c@(Small "end") adjacencyList visited = [[c]]
solve1' c               adjacencyList visited = 
  let adj = [ to | (from, to) <- adjacencyList, from == c, to `notElem` visited]
  in concatMap (\x -> map (c:) $ solve1' x adjacencyList (bool visited (c : visited) $ isSmall c)) adj

-- | Solution for part 2
solve2 :: [Edge] -> Int
solve2 es = length $ solve2' (Small "start") False es [Small "start"]
 where
  solve2' :: Cave String -> Bool -> [Edge] -> [Cave String] -> [Path]
  solve2' c@(Small "end") _     _             _       = [[c]]
  solve2' c               True  adjacencyList visited = solve1' c adjacencyList visited
  solve2' c               t     adjacencyList visited =  
    let adj = [ to | (from, to) <- adjacencyList, from == c, to /= Small "start"]
    in concatMap (\x -> map (c:) $ solve2' x (x `elem` visited) adjacencyList (bool visited (c : visited) $ isSmall c)) adj
