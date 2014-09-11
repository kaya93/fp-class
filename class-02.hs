-- 1.1
-- Написать функцию, которая разбивает промежуток времени в секундах на часы, минуты и секунды.
-- Результат возвращать в виде кортежа из трёх элементов. Реализовать также обратное преобразование.
sec2hms :: Int -> (Int, Int, Int)
sec2hms sec = (hours, minutes, seconds)
	where
	  hours = sec `div` 3600
	  minutes = ( sec `mod` 3600 ) `div` 60
	  seconds = ( sec `mod` 3600 ) `mod` 60

hms2sec :: (Int, Int, Int) -> Int
hms2sec (h, m, s) = s + m * 60 + h * 3600

-- Реализовать с помощью hms2sec (здесь параметры заданы по отдельности)
hms2sec' :: Int -> Int -> Int -> Int
hms2sec' h m s = hms2sec (h,m,s)

-- должно быть True
test1 = and $ map (\x -> x == hms2sec (sec2hms x)) [1,10..10000]
-- *Main> test1  -- результат
--True

-- 1.2
-- Написать функции, вычисляющие
-- а) длину отрезка по координатам его концов;
-- б) периметр и площадь треугольника по координатам вершин.

type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt ( (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1) )

triangle :: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double)
triangle (x1,y1) (x2,y2) (x3,y3) = (p, s)
  where
    p = distance (x1,y1) (x2,y2) + distance (x1,y1) (x3,y3) + distance (x2,y3) (x3,y3)
    s = abs ( (x2-x1) * (y3-y1) - (x3-x1) * (y2-y1) ) / 2

-- Во всех следующих заданиях использование стандартных функций обработки списков не допускается.
-- Все решения должны реализовываться рекурсивными функциями.

-- 2.1
-- Определить рекурсивную функцию, определяющую количество чётных элементов списка
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs) 
	| even x = 1 + nEven xs
	| otherwise = nEven xs

-- 2.2
-- Увеличить все элементы заданного списка в два раза.
-- Указание: в решении может понадобиться операция конструирования списка:
-- > 1 : [2,3,4]
--   [1,2,3,4]
doubleElems :: Num a => [a] -> [a]
doubleElems [] = []
doubleElems (x:xs) = x*2 : doubleElems xs

-- 2.3
-- Дан список целых чисел. Сформировать новый список, содержащий только нечетные элементы исходного.
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs) 
	| odd x = x : fltOdd xs
	| otherwise = fltOdd xs

-- 2.4
-- Написать следующие функции обработки списков:
-- а) удалить все отрицательные элементы;
delNegatives :: (Num a, Ord a) => [a] -> [a]
delNegatives [] = []
delNegatives (x:xs) 
	| (x > 0) || (x == 0) = x : delNegatives xs
	| otherwise = delNegatives xs

-- б) увеличить элементы с чётными значениями в два раза;
doubleEvens :: Integral a => [a] -> [a]
doubleEvens [] = []
doubleEvens (x:xs)
	| even x = x*2 : doubleEvens xs
	| otherwise = x : doubleEvens xs

-- в) переставить местами чётные и нечётные по порядку следования элементы
--    (для списков нечётной длины отбрасывать последний элемент).
reverseOEElems :: Eq a => [a] -> [a]
reverseOEElems [] = []
reverseOEElems xs
	| even (lengthList xs) = rev xs
	| otherwise = rev ( dropLast xs )
	where
	  rev [] = []
	  rev (x:y:xs) = y : x: rev xs
          lengthList [] = 0
          lengthList (x:xs) = 1 + lengthList xs
          dropLast [] = []
          dropLast (x:xs) 
	    | xs /= [] = x : dropLast xs
	    | otherwise = dropLast xs

-- 2.5 
-- Даны два списка целых чисел. Сформировать список, каждый элемент которого равен сумме
-- соответствующих   элементов исходных списков. Предусмотреть ситуацию списков разной длины.
combine_plus :: [Integer] -> [Integer] -> [Integer]
combine_plus [] ys = ys
combine_plus xs [] = xs
combine_plus (x:xs) (y:ys) 
	| (lengthList xs) > (lengthList ys) && ys /= [] = x+y : combine_plus xs ys
	| (lengthList xs) < (lengthList ys) && xs/= [] = x+y : combine_plus xs ys
	| otherwise = x+y : combine_plus xs ys
        where
	  lengthList [] = 0
	  lengthList (x:xs) = 1 + lengthList xs
-- 2.6
-- Даны два списка. Сформировать новый список, содержащий пары из соответствующих элементов
-- исходных списков. Хвост более длинного списка отбросить.
combinetoPairs xs [] = []
combinetoPairs [] ys = []
combinetoPairs (x:xs) (y:ys) 
	| (lengthList xs) > (lengthList ys) && ys /= [] = (x,y) : combinetoPairs xs ys
	| (lengthList xs) < (lengthList ys) && xs/= [] = (x,y) : combinetoPairs xs ys
	| otherwise = (x,y) : combinetoPairs xs ys
        where
	  lengthList [] = 0
	  lengthList (x:xs) = 1 + lengthList xs

-- 2.7
-- Написать функции, которые по заданному n возвращают список, состоящий из n первых натуральных чисел
-- а) в порядке убывания;
firstNR :: (Eq a, Num a) => a -> [a]
firstNR 0 = []
firstNR n = n : firstNR ( n - 1 )

-- б) в порядке возрастания.
firstN :: (Eq a, Num a, Num a1) => a -> [a1]
firstN n = first n 1
	where
	  first 0 i = []
	  first n i = i : first (n-1) (i+1)
-- 2.8
-- Дан элемент типа a и список [a]. Вставить между всеми элементами списка заданный элемент.
insert :: Eq a => a -> [a] -> [a]
insert a [] = []
insert a (x:xs) 
	| xs /= [] = x : a : insert a xs 
	| otherwise = x : insert a xs
-- 2.9
-- Написать функцию, которая разбивает список на два подсписка: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные элементы, например:
-- [1,1,1,2,3,1] -> ([1,1,1], [2,3,1]).

--3
-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают, и напишите их
-- рекурсивные реализации (если вы можете предложить несколько вариантов, реализуйте все):
-- а) [a] -> Int -> a
-- б) Eq a => [a] -> a -> Bool
-- в) [a] -> Int -> [a]
-- г) a -> Int -> [a]
-- д) [a] -> [a] -> [a]
-- е) Eq a => [a] -> [[a]]
-- ж) [a] -> [(Int, a)]
-- з) Eq a => [a] -> [a]
