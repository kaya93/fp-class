{-
  Дан текстовый файл (его имя задано в параметрах командной строки), содержащий целые числа
  в диапазоне от 1 до 1000, разделённые пробелами и символами перевода строки. Определить
  количество различных чисел в нём, пользуясь для этого возможностями различных структур
  данных. 
-}

import Data.List 
import qualified Data.Sequence as Seq 
import qualified Data.IntSet as Set 
import Data.Array.IArray 
import Data.Array.IArray as Iarray 
import System.Environment 
import Control.Monad 

nub_set :: Set.IntSet -> Int
nub_set = Set.size

nub_list :: [Int] -> Int 
nub_list = length  

nub_seq :: Seq.Seq a -> Int
nub_seq (xs) = Seq.length (xs)

nub_arr :: Array Int Int -> Int 
nub_arr (xs) = length $ Iarray.elems xs 

main = do
  [fname] <- getArgs
  content <- readFile fname
  let xs = map read $ concatMap words $ lines content
  let (n:results) = [
        nub_set $ Set.fromList xs,
        nub_list xs,
        nub_seq $ Seq.fromList xs,
        nub_arr $ listArray (1,length xs) xs ]
  mapM_ print results
  when (any (/= n) results) $ putStrLn "Результаты не совпадают!"
