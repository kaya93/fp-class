{-
  Создать текстовый файл, содержащий случайные целые числа, разделённые пробелами
  и символами перевода строки. В командной строке должны задаваться следующие параметры:
  1) имя создаваемого файла;
  2) диапазон генерируемых случайных чисел: от и до;
  3) количество чисел в строке;
  4) количество строк в файле.
-}

import System.Random
import System.Environment

main = do
	[file, from, to, n, str] <- getArgs
	genRandomInts file (read from) (read to) (read n) (read str)

genStringFile from to n = do
	gen1 <- newStdGen
	return (concat ((map (\x -> x ++ " ") $ map show $ take n $ (randomRs (from, to) gen1 :: [Int]))++["\n"]))
	
genRandomInts file from to n strs = do
	if strs > 0
	then do
		str <- genStringFile from to n  
		appendFile file (str)
		genRandomInts file from to n (strs - 1)
	else
		return ()
