import Data.Bifunctor (first, second)

main :: IO ()
main =  readFile ".\\input.txt"  >>= print . solve2 . map pairs . lines

example :: [Direction Int]
example = map pairs ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]

data Direction a = F a | D a | U a

pairs :: String -> Direction Int
pairs s = case words s of 
 ("forward": n : _) -> F $ read n
 ("down"   : n : _) -> D $ read n
 ("up"     : n : _) -> U $ read n
 _                  -> error ""

-- | Could be derived 
foldDirection :: (a -> b) -> (a -> b) -> (a -> b) -> Direction a -> b
foldDirection f d u da = case da of
    F x -> f x
    D x -> d x
    U x -> u x

-- | Solution for first part
solve :: [Direction Int] -> Int
solve = uncurry (*) . foldr (foldDirection (first . (+)) (second . (+)) (second . subtract)) (0, 0)

-- | Solution for second part
solve2 :: [Direction Int] -> Int
solve2 = (\(x,y,z) -> x * y) . foldl (flip fold) (0, 0, 0)
 where
  fold :: Direction Int -> (Int, Int, Int) -> (Int, Int, Int)
  fold = foldDirection (\n (x,y,z) -> (x + n, y + z * n, z)) (second . (+)) (second . subtract)

-- | A simple solution without extensive use of higher order
notFold :: [Direction Int] -> Int
notFold = uncurry (*) . fst . notFold' 0 0 0
 where
  notFold' :: Int -> Int -> Int -> [Direction Int] -> ((Int, Int), Int)
  notFold' currH currD currA []     = ((currH, currD), currA)
  notFold' currH currD currA (x:xs) = case x of
    F n -> notFold' (currH + n) (currD + currA * n) currA xs
    D n -> notFold' currH currD (currA + n) xs
    U n -> notFold' currH currD (currA - n) xs
