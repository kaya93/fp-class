{-# LANGUAGE EmptyDataDecls #-}

module Drunkard where

{-
  1. Определить типы данных, необходимые для представления игральной карты в игре «Пьяница»,
  учитывая, что всего в колоде 52 карты.
-}

data Suit = S | C | D | H deriving (Show, Eq)

data Value = Two | Thr | Fou | Fiv | Six | Sev | Eig | Nin | Ten | J | Q | K | A deriving (Show, Eq, Ord)

data Card = Card Value Suit deriving (Show, Eq)

c_suit:: Card -> Suit
c_suit (Card _ s) = s

c_value:: Card -> Value
c_value (Card v _) = v

-- 2. Определить функцию, проверяющую, что две переданные ей карты одной масти.

sameSuit :: Card -> Card -> Bool
sameSuit c1 c2
	| c_suit c1 == c_suit c2 = True
	| otherwise = False

{-
  3. Определить функцию, проверяющую, что переданная ей первой карта старше второй
  (масть в игре «Пьяница» игнорируется). Возвращённое значение EQ означает, что обе
  карты одинакового старшинства.
-}

beats :: Card -> Card -> Ordering
c1 `beats` c2 = compare (c_value c1) (c_value c2)

{-
  4. Определить функцию, которая по паре списков карт возвращает новую пару списков карт
  с учетом правил игры «Пьяница» (один раунд игры): 
    * из вершин списков берутся две карты и добавляются в конец того списка, карта из
      которого старше оставшейся;
    * если первые взятые карты совпадают по достоинству, то из списков берутся и
      сравниваются следующие две карты (и так до тех пор, пока не будет определён победитель
      раунда).
-}

game_round :: ([Card], [Card]) -> ([Card], [Card])
game_round ([],cards2) = ([],cards2)
game_round (cards1, []) = (cards1,[])
game_round (cards1,cards2)
	| (head cards1) `beats` (head cards2) == GT = ((tail cards1)++[(head cards2)],(tail cards2))
	| (head cards1) `beats` (head cards2) == LT = ((tail cards1), (tail cards2)++[(head cards2)])
	| otherwise = game_round ((tail cards1),(tail cards2));

{-
  5. Определить функцию, которая по паре списков возвращает количество раундов, необходимых
  для завершения игры (одна из колод оказывается пустой), и номер победителя.
-}

data Winner = First | Second deriving (Show,Eq)

game :: ([Card], [Card]) -> (Winner, Int)
game cards = game' (cards, 0)
	where
	  game' (([], _), rounds) = (Second, rounds)
	  game' ((_, []), rounds) = (First, rounds)
	  game' (cards, rounds) = game' (game_round cards, rounds+1)

{-
  6. Приведите здесь результаты как минимум пяти запусков функции game (в каждом списке
  изначально должно быть не менее 10 карт).
-}

{-
В каждом списке по 12 карт

*Drunkard> game ([(Card J S),(Card Ten D),(Card Two S),(Card Six H),(Card Ten C),(Card Fou D),(Card Eig D),(Card A H),(Card Eig H),(Card Nin D),(Card J C),(Card Q S)],[(Card K D),(Card Nin S),(Card Ten H),(Card A S),(Card Fou C),(Card Sev C),(Card Q H),(Card A C),(Card Two C),(Card Fiv H),(Card Ten C),(Card K C)])
(Second,16)

*Drunkard> game ([(Card J S),(Card Ten D),(Card Two S),(Card Six H),(Card Ten C),(Card Fou D),(Card Eig D),(Card Two H),(Card Eig H),(Card Nin D),(Card J C),(Card Q S)],[(Card K D),(Card Nin S),(Card Ten H),(Card A S),(Card A C),(Card Sev C),(Card Q H),(Card Ten H),(Card Two C),(Card K H),(Card Two D),(Card K C)])
(Second,15)

*Drunkard> game ([(Card J S),(Card Ten D),(Card Two S),(Card Six H),(Card Ten C),(Card Fou D),(Card Eig D),(Card Two H),(Card Eig H),(Card Nin D),(Card J C),(Card Q S)],[(Card K D),(Card Nin S),(Card Ten H),(Card Thr S),(Card Six C),(Card Sev C),(Card Six D),(Card Ten H),(Card Two C),(Card K H),(Card Two D),(Card Thr D)])
(Second,19)

*Drunkard> game ([(Card Thr H),(Card Eig S),(Card Two S),(Card Six H),(Card Ten C),(Card Fou D),(Card Eig D),(Card Two H),(Card Eig H),(Card Nin D),(Card Fiv C),(Card Thr S)],[(Card K D),(Card Nin S),(Card Ten H),(Card A S),(Card A C),(Card Sev C),(Card Q H),(Card Ten H),(Card Two C),(Card K H),(Card Two D),(Card K C)])
(Second,14)

*Drunkard> game ([(Card A C),(Card J D),(Card Q S),(Card Ten H),(Card K C),(Card Fou D),(Card Eig D),(Card Two H),(Card Eig H),(Card Nin D),(Card Fiv C),(Card Fou S)],[(Card Two D),(Card Nin S),(Card Ten H),(Card Thr S),(Card Q C),(Card K C),(Card A D),(Card A H),(Card J C),(Card K H),(Card J H),(Card Q D)])
(Second,17)
-}

{-
  7 (необязательное упражнение). Реализуйте версию функции game, которая помимо результатов
  игры возвращает запись всех ходов (карты, выкладываемые по ходу игры для сравнения).
-}

{-
  8 (необязательное упражнение). При выполнении функций из упражнений 4 и 5 возможно
  зацикливание. Чтобы его избежать, можно предусмотреть максимальное количество повторений
  (для раундов и ходов в рамках одного раунда). Подумайте, как обнаружить факт зацикливания
  в функции 4? Можно ли применить такой же подход в функции 5? Что нужно возвращать в случае
  обнаружения факта зацикливания? Измените соответствующим образом типовые аннотации и
  напишите безопасные по отношению к зацикливанию версии функций game_round и game.
-}
