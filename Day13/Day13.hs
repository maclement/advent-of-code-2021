{-# LANGUAGE DeriveFunctor #-}
import Data.Bifunctor ( bimap, second )
import Data.List ( groupBy, transpose, partition )
import Data.Function ( on )
import Data.Char ( isAlpha, isNumber )
import Data.Bool ( bool )

main :: IO ()
main =  readFile ".\\input.txt" >>= mapM_ putStrLn . solve2 . toInput . lines

example :: ([[Bool]], [Instruction Int])
example = toInput [ "6,10","0,14","9,10","0,3","10,4","4,11","6,0","6,12","4,1","0,13","10,12","3,4","3,0","8,4","1,10","2,14","8,10","9,0"
                  , "" , "fold along y=7", "fold along x=5" ]

data Instruction a = X a | Y a
 deriving (Functor, Show)

toInput :: [String] -> ([[Bool]], [Instruction Int])
toInput ss = bimap toArr (map toInst . tail) $ break null ss
 where
  toArr :: [String] -> [[Bool]]
  toArr ss = let pairs = map toPair ss
                 (maxX, maxY) = bimap maximum maximum $ unzip pairs 
             in [ [ (i, j) `elem` pairs  
                  | j <- [0 .. maxY]
                  ] 
                | i <- [0 .. maxX]
                ]
  toPair :: String -> (Int, Int)
  toPair s = let xs = filter (/= ",") . groupBy ((&&) `on` isNumber) $ s in (read $ head xs, read $ (head . tail) xs)
  
  toInst :: String -> Instruction Int
  toInst s = let e = drop 11 s 
                 f = read . head . filter (all isNumber) . groupBy ((&&) `on` isNumber)
             in case head e of
      'x' -> X $ f e 
      'y' -> Y $ f e
      _ -> undefined


solve1 :: ([[Bool]], [Instruction Int]) -> Int
solve1 (bs, [])   = error "not needed"  
solve1 (bs, i:is) = length $ filter id $ concat (step bs i)

step :: [[Bool]] -> Instruction Int -> [[Bool]]
step bs (Y a) = transpose $ step (transpose bs) (X a) 
step bs (X a) = uncurry (zipWith' (zipWith (||))) (second (reverse . tail) $ splitAt a bs)

zipWith' :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith' _ []     xs     = xs
zipWith' _ xs     []     = xs
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

solve2 :: ([[Bool]], [Instruction Int]) -> [String]
solve2 (bs, [])   = map (map (bool '_' '#')) (transpose bs) 
solve2 (bs, i:is) = solve2 (step bs i, is)