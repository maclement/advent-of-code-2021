import Data.Maybe ( listToMaybe )
import Data.List ( sort )
import Debug.Trace

main :: IO ()
main =  readFile ".\\input.txt" >>= print . solve2 . lines

-- | Solution to task 1
solve1 :: [String] -> Int
solve1 =  sum . map (score . head) .  filter (not . null) . map (`tryMatch` [])
 where
  score :: Char -> Int
  score c = case c of
   '>' -> 25137 
   ')' -> 3
   '}' -> 1197
   ']' -> 57
   _   -> 0

tryMatch :: String -> [Char] -> [Char]
tryMatch []     s = []
tryMatch (x:xs) s | isOpen   x = tryMatch xs (flipB x:s)
                  | isClosed x = maybe [] (\y -> if y == x then tryMatch xs (tail s) else x:s) (listToMaybe s)
                  | otherwise  = error "Meh"



-- | Solution to task 2
solve2 :: [String] -> Int
solve2 = (\xs -> xs !! (length xs `div` 2)) . sort  . filter (>0) . map (valueS . flip tryMatch2 [])

valueS :: String -> Int
valueS = foldl (\acc x -> value x + acc * 5) 0
 where
  value :: Char -> Int
  value c = case c of
   ')' -> 1
   ']' -> 2
   '}' -> 3
   '>' -> 4
   _   -> error "Meeeeeh"

tryMatch2 :: String -> [Char] -> [Char]
tryMatch2 []     s =  s
tryMatch2 (x:xs) s | isOpen   x = tryMatch2 xs (flipB x:s)
                   | isClosed x = maybe (trace "lul" []) (\y -> if y == x then tryMatch2 xs (tail s) else []) (listToMaybe s)
                   | otherwise  = error "Meh^2"

isOpen :: Char -> Bool
isOpen = flip elem "({[<"

isClosed :: Char -> Bool
isClosed = flip elem ")}]>"

flipB :: Char -> Char
flipB c = case c of
  '<' -> '>'
  '(' -> ')'
  '[' -> ']'
  '{' -> '}'
  '>' -> '<'
  ')' -> '('
  ']' -> '['
  '}' -> '{'
  x   -> x