example :: [Int]
example = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

main :: IO ()
main =  readFile ".\\input2.txt"  >>= print . snd . numOfIncreasesFold . summarize . map read . lines

summarize :: [Int] -> [Int]
summarize v = let z1 = zipWith (+) v  (drop 1 v)
              in zipWith (+) z1 (drop 2 v) 

numOfIncreasesFold :: [Int] -> (Int, Int)
numOfIncreasesFold = foldl (\(v, c) x -> (x, if x > v then c + 1 else c)) (0, -1)