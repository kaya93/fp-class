{-
  Разработайте утилиту со следующими возможностями:
  1) подсчёт количества строк в заданном текстовом файле;
  2) добавление заданной строки в начало (конец) заданного файла;
  3) преобразование всех буквенных символов заданного файла к верхнему
     регистру (результат выводится на консоль);
  4) построчное слияние двух заданных файлов (каждая строка первого файла
     соединяется с соответствующей строкой второго файла);
  5) генерация случайного текстового файла (случайность должна ограничиваться
     максимальным количеством строк в файле и символов в строке).

  Все входные данные программы должны передаваться с помощью параметров
  командной строки.
-}
import System.Environment
import System.Directory
import Data.List
import Data.Char
import System.IO
--import System.Random

main = do
	(n_task : fname : args) <- getArgs
	act n_task fname args

act n_task fname args
	| n_task == "1" = count_strs fname
	| n_task == "2_1" = append_f fname args
	| n_task == "2_2" = p_append_f fname args
	| n_task == "3" = to_upper fname
	| n_task == "4" = string_merge fname args

count_strs fname = do
	strs <- readFile fname
	putStrLn (show (length (lines strs)))

append_f fname [s] = appendFile fname ("\n" ++ s)

p_append_f fname [s] = do
	strs <- readFile fname
	let tF = ("dot" ++ fname)
	writeFile tF (s ++ "\n")
	writeFile tF (strs)
	renameFile tF fname

to_upper fname = do
	strs <- readFile fname
	let tF = ("dot" ++ fname)
	let strs' = map toUpper strs
	writeFile tF strs'
	renameFile tF fname

string_merge fname1 [fname2] = do
	strs1 <- readFile fname1
	strs2 <- readFile fname2
	let tF = "new_f.txt"
	writeFile tF $ unlines $ zipWith (++) (lines strs1) (lines strs2)
