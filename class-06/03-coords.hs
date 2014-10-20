{-
  Написать программу, которая в зависимости от параметров командной строки
  а) генерирует случайный текстовый файл содержащий декартовы координаты точек на плоскости
     (по одной точке в каждой строке);
  б) определяет по заданному файлу в указанном ранее формате количество точек в каждой
     из четвертей;
  в) отыскивает наиболее удалённую от начала координат точку.
-}
--import System.Environment
--import System.Directory
--import Data.List
--import Data.Char
import System.Random
import System.Environment

data Point = Point { x :: Double, y :: Double } deriving (Eq,Show)

main = undefined
