import Control.Concurrent.MVar
import Control.Monad
import Control.Applicative
import Data.List
import Data.Array
import Debug.Trace

data BinTree = Nil | Node Int BinTree BinTree

instance Show BinTree where
  show t =
    intercalate " " $ map show $ go t []
    where
      go :: BinTree -> [Int] -> [Int]
      go Nil acc = acc
      go (Node i l r) acc = go l (i : (go r acc))

type Nodes = Array Int BinTree

main = do
  n <- readLn
  nds <- forM [1 .. n] $ \i -> do
    [l, r] <- map read . words <$> getLine
    return (i, l, r)
  let root = initTree nds
  t <- readLn
  ds <- replicateM t readLn
  let ans = tail $ scanl swapAt root ds
  mapM_ print ans

at :: Nodes -> Int -> BinTree
nodes `at` -1 = Nil
nodes `at` i  = nodes ! i

initTree :: [(Int, Int, Int)] -> BinTree
initTree nds =
  let
    nodes = array (1, length nds) $ map (\(i, l, r) -> (i, Node i (nodes `at` l) (nodes `at` r))) nds
  in
    nodes ! 1

swapAt :: BinTree -> Int -> BinTree
swapAt root d =
  update root 1
  where
    update :: BinTree -> Int -> BinTree
    update Nil _ = Nil
    update (Node i l r) t
      | t `mod` d == 0 = Node i (update r (t + 1)) (update l (t + 1))
      | otherwise      = Node i (update l (t + 1)) (update r (t + 1))
