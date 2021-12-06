import Data.Char (isNumber)
import Data.List (group, sort)

main :: IO ()
main =  readFile ".\\input.txt" >>= print . solve 256 . toInput . lines


newtype Swarm = Swarm { unSwarm :: (Int, Int, Int, Int, Int, Int, Int, Int, Int) }
 deriving Show

solve :: Int -> [Int] -> Int
solve i = sumSwarm . flip (!!) i . iterate  update . toSwarm

sumSwarm :: Swarm -> Int
sumSwarm (Swarm (a, b, c, d, e, f, g, h, i)) = sum [a, b, c, d, e, f, g, h, i] 

toInput :: [String] -> [Int]
toInput = concatMap (map (read . return) . filter isNumber)

update :: Swarm -> Swarm
update (Swarm (z, o, tw, th, fo, fi, si, se, ei)) = Swarm (o, tw, th, fo, fi, si, se + z, ei, z)

example :: [Int]
example = [3,4,3,1,2]

toSwarm :: [Int] -> Swarm
toSwarm is = let [a,b,c,d,e,f,g,h,i] = map (length . tail) . group . sort . (++) [0..8] $ is 
             in Swarm (a,b,c,d,e,f,g,h,i)