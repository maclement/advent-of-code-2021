import Data.Maybe ( fromJust, isJust )
import Data.Bool ( bool )
import Data.Char ( isAlpha, ord )
import Data.List ( groupBy, sort, splitAt, elemIndex, nub, permutations) 
import Data.Bifunctor ( bimap )
import Data.Function ( on )
import Control.Applicative ((<|>))


main :: IO ()
main =  readFile ".\\input.txt" >>= print . solve . toInput . lines
 
example :: [([String], [String])]
example = toInput $ return "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

-- | Solution for b
solve :: [([String], [String])] -> Int
solve = sum . map (toDec . map sort . solveRow)
 where
  solveRow :: ([String], [String]) -> [String]
  solveRow (lhs, rhs) = let m = head $ filter (`isValidMapping` lhs) allMappings
                        in  map (map (fromJust . m)) rhs
  toDec :: [String] -> Int
  toDec = foldl (\num s -> toNum s + 10 * num) 0

  toNum :: String -> Int
  toNum s = case s of 
    "abcefg"  -> 0
    "cf"      -> 1
    "acdeg"   -> 2 
    "acdfg"   -> 3
    "bcdf"    -> 4
    "abdfg"   -> 5
    "abdefg"  -> 6
    "acf"     -> 7
    "abcdefg" -> 8
    "abcdfg"  -> 9
    s -> error $ "got: " ++ s 

toInput :: [String] -> [([String], [String])]
toInput = map toNumbers
 where
  toNumbers :: String -> ([String], [String])
  toNumbers s = let f = map sort . filter (/= "|") . filter (/= " ") . groupBy ((&&) `on` isAlpha )
                 in  bimap f f $ splitAt (fromJust $ elemIndex '|' s) s

type Mapping = Char -> Maybe Char

valid :: [String]
valid = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

allMappings :: [Mapping]
allMappings = toMap <$> permutations ['a'..'g']
 where
  toMap :: String -> Mapping
  toMap s = foldr combineM emptyM $ zipWith (\x y x' -> if x == x' then Just y else Nothing) ['a'..'g'] s

emptyM :: Mapping
emptyM = const Nothing

combineM :: Mapping -> Mapping -> Mapping
combineM m1 m2 c = m1 c <|> m2 c

isValidMapping :: Mapping -> [String] -> Bool
isValidMapping m = all isValid
 where
  isValid :: String -> Bool
  isValid s = let s' = sort . map (fromJust . m) $ s in elem s' valid