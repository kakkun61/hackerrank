import Control.Concurrent.MVar
import Control.Monad
import Control.Applicative
import Data.List

data BinTree = Nil | BinTree Int (MVar BinTree) (MVar BinTree)

main = do
  n <- readLn
  trees <- newTrees n
  let root = head trees
  forM_ [0 .. n - 1] $ \i -> do
    [l, r] <- map ((\n -> n - 1) . read) . words <$> getLine
    append trees i l r
  d <- depth root
  t <- readLn
  replicateM_ t $ do
    k <- readLn
    mapM_ (swapAt root) [k, 2 * k .. d]
    printTreesLine root

newTrees :: Int -> IO [BinTree]
newTrees n =
  forM [1 .. n] $ \i -> do
    l <- newMVar Nil
    r <- newMVar Nil
    return $ BinTree i l r

append :: [BinTree] -> Int -> Int -> Int -> IO ()
append trees it il ir = do
  let t = trees !! it
  case t of
    Nil -> error "Nil tree"
    BinTree i ml mr -> do
      unless (il == -2) $ do
        takeMVar ml
        putMVar ml (trees !! il)
      unless (ir == -2) $ do
        takeMVar mr
        putMVar mr (trees !! ir)

swapAt :: BinTree -> Int -> IO ()
swapAt root d = do
  ts <- atDepth root d
  mapM_ swap ts

swap :: BinTree -> IO ()
swap (BinTree i ml mr) = do
  l <- takeMVar ml
  r <- takeMVar mr
  putMVar ml r
  putMVar mr l
  return ()

depth :: BinTree -> IO Int
depth Nil = return 0
depth (BinTree i ml mr) = do
  l <- readMVar ml
  r <- readMVar mr
  dl <- depth l
  dr <- depth r
  return $ 1 + (max dl dr)

atDepth :: BinTree -> Int -> IO [BinTree]
atDepth root d =
  go [] root d
  where
    go :: [BinTree] -> BinTree -> Int -> IO [BinTree]
    go acc Nil _ = return acc
    go acc tree 1 = return $ tree:acc
    go acc (BinTree i ml mr) d = do
      l <- readMVar ml
      r <- readMVar mr
      lacc <- go acc l (d - 1)
      go lacc r (d - 1)

printTreesLine :: BinTree -> IO ()
printTreesLine root = showTrees root >>= putStrLn

showTrees :: BinTree -> IO String
showTrees root = do
  is <- map show <$> go [] root
  return $ intercalate " " is
  where
    go :: [Int] -> BinTree -> IO [Int]
    go acc (BinTree i ml mr) = do
      l <- readMVar ml
      r <- readMVar mr
      racc <- go acc r
      go (i:racc) l
    go acc Nil = return acc
