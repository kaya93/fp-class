{-
  Соберите следующую информацию по текстовому файлу kapitan.txt:

  1) самый часто используемый знак препинания;
  2) 50 наиболее часто используемых в тексте слов (с указанием количества использований);
  3) частоты символов, биграмм и триграмм (вывести соответствующую информацию для
     наиболее часто встречающихся);
  4) количества использованных предлогов, местоимений и имён собственных.

  Постарайтесь использовать в решении наиболее подходящие структуры данных, избегайте повторные
  вычисления и заведомо неэффективные операции. Проверьте написанную программу на трёх других
  текстах того же или большего размера. Сравните результаты.
-}
import Data.Char 
import Data.List 
import Data.Function (on) 
import qualified Data.Map as Map 
import System.Environment 

growWdValue key map = case (Map.lookup key map) of
	Just x -> Map.update (\val -> Just (val + 1)) key map
	Nothing -> Map.insert key 0 map
	
map' text = foldl (\acc x -> if (isPunctuation x) then (growWdValue x acc) else acc) Map.empty text
maxim mp = maximum ( Map.elems mp)
mostOftPunct text = Map.keys (Map.filter (==maxim (map' text)) (map' text))


main = do
	[fname] <- getArgs
	content <- readFile fname
	putStrLn ("First task:"++(show (mostOftPunct content)))
