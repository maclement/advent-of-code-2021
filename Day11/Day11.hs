import Data.Maybe (catMaybes, mapMaybe)
import Control.Monad.State
import Data.Functor ( (<&>) )
import Data.List ( partition, group )
import Data.Bifunctor


main :: IO ()
main =  readFile ".\\input.txt" >>= print .  solve2 . lines

toInput :: [String] -> [[Int]]
toInput = map (map (read . return))

example :: [String]
example = ["5483143223"
          ,"2745854711"
          ,"5264556173"
          ,"6141336146"
          ,"6357385478"
          ,"4167524645"
          ,"2176841721"
          ,"6882881134"
          ,"4846848554"
          ,"5283751526"]

width :: [[a]] -> Int
width = length . head

height :: [[a]] -> Int
height = length

-- | Save lookup
(!?) :: [a] -> Int -> Maybe a
xs !? i | i < 0      = Nothing
        | otherwise  = go xs i
 where
  go :: [a] -> Int -> Maybe a
  go (x:_)  0 = Just x
  go []     _ = Nothing
  go (_:xs) n = go xs (n - 1)

type Index = (Int, Int)

-- | Save 2dim lookup
(!!?) :: [[a]] -> Index -> Maybe a
(!!?) xss (i, j) = xss !? i >>= (!? j)
 
-- | All adjacent elements to a given index
adj :: [[a]] -> Index -> [(Index, a)]
adj xss (i, j) = mapMaybe (\p -> (,) p <$> xss !!? p) [(op1 i, op2 j) | op1 <- [succ, pred, id], op2 <- [succ, pred, id]]

zip2dim :: [[a]] -> [[b]] -> [[(a, b)]]
zip2dim = zipWith zip 

-- | Alters a value in a list at given position with a predicate
modifyAt :: Int -> (a -> a) -> [a]  -> [a]
modifyAt  0 f (x:xs) = f x : xs
modifyAt  n f (x:xs) = x : modifyAt (pred n) f xs
modifyAt  _ _ [] = []

-- | All indices of a 2-dimensional list
indices :: [[a]] -> [[Index]]
indices xss =  flip zip [0..pred $ width xss] <$> map (replicate (succ $ width xss)) [0..pred $ height xss]

-- | One update step
update :: State [[Int]] Int
update = do 
  modify (map . map $ (+1)) 
  x <- buildStack >>= consumeStack
  modify (map (map (\y -> if y > 9 then 0 else y))) 
  return x

buildStack :: State [[Int]] [Index]
buildStack = (gets indices >>= gets . zip2dim) <&> concatMap (map fst . filter ((>9) . snd))

-- | Consumes stored flashes and counts the number of flashes
consumeStack :: [Index] -> State [[Int]] Int
consumeStack []     = return 0
consumeStack (i:is) = succ <$> (flash i >>= consumeStack . (++ is))

-- | Emits a flash at a given position.
flash :: Index -> State [[Int]] [Index]
flash i = get >>= flashRec . partition ((9 ==) . snd) . flip adj i
 where
  flashRec :: ([(Index, Int)], [(Index, Int)]) -> State [[Int]] [Index]
  flashRec (toDo, toUpdate) = mapM_ (incAtS . fst) toUpdate >> mapM_ (incAtS . fst) toDo >> return (map fst toDo)
    
incAtS :: Index -> State [[Int]] ()
incAtS (i, j) = modify $ modifyAt i (modifyAt j succ)

solve1 :: [String] -> Int
solve1 s = evalState (updateN 100) (toInput s)
 where
  updateN :: Int -> State [[Int]] Int
  updateN 0 = return 0
  updateN n = (+) <$> update <*> updateN (pred n)


solve2 :: [String] -> Int
solve2 s = evalState updateWhileUneq (toInput s)
 where
  updateWhileUneq :: State [[Int]] Int
  updateWhileUneq = gets ((==1) . length . group . concat) >>= \b -> if b then return 0 else succ <$> (update >> updateWhileUneq)