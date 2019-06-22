module List where

import Data.Char (isUpper)
import Data.List

{- [] и : - два основных конструктора списков -}

lst :: [Integer]
lst = 1 : 2 : 3 : 4 : 5 : []

add42 :: [Integer] -> [Integer]
add42 = ((:) 42)

nTimes:: t -> Integer -> [t]
nTimes e n | n == 0    = []
           | n > 0     = e : nTimes e (pred n)
           | otherwise = error "Parameter n must be positive"

{-
  head возвращает головной элемент списка
  tail возвращает хвост списка без головного элемента
-}

snd' :: [t] -> t
snd' = head . tail

head' :: [t] -> t
head' ((:) x xs) = x

tail' :: [t] -> [t]
tail' (x : xs) = xs

snd'' :: [t] -> t
snd'' (_ : x : _) = x

len :: [t] -> Integer
len []       = 0
len (x : xs) = 1 + len xs

infixr 5 +++
(+++) :: [a] -> [a] -> [a]
[] +++ ys       = ys
(x : xs) +++ ys = x : xs +++ ys

{-
null :: [a] -> Bool проверяет, является ли список пустым
odd проверяет, является ли число нечетным
even проверяет, является ли число четным
-}


oddsOnly :: Integral a => [a] -> [a]
oddsOnly []       = []
oddsOnly (x : xs) = if odd x then x : oddsOnly xs else oddsOnly xs

{-
last возвращает последний элемент списка
init возвращает весь список без последнего элемента
-}

last' :: [a] -> a
last' (x : []) = x
last' (_ : xs) = last' xs

init' :: [a] -> [a]
init' (x : [_]) = [x]
init' (x : xs)  = x : init' xs

{-
sum, product :: (Num t) => [t] -> t
maximum, minimum :: (Ord t) => [t] -> t
reverse :: [t] -> [t] - разворачивает список
zip :: [t] -> [v] -> [(t, v)]
zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
unzip :: [(a, b)] -> ([a], [b])
-}

zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' (x : xs) (y : ys) (z : zs) = (x, y, z) : zip3' xs ys zs
zip3' _        _        _        = []

unzip' :: [(a, b)] -> ([a], [b])
unzip' []             = ([], [])
unzip' ((x, y) : xys) = (x : xs, y : ys)
  where (xs, ys) = unzip' xys

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 xs ys zs = (x + y + z) : sum3 nxs nys nzs
  where
    (x, nxs) = if null xs then (0, xs) else (head xs, tail xs)
    (y, nys) = if null ys then (0, ys) else (head ys, tail ys)
    (z, nzs) = if null zs then (0, zs) else (head zs, tail zs)

sum3' :: Num a => [a] -> [a] -> [a] -> [a]
sum3' []       []       []       = []
sum3' []       ys       zs       = sum3' [0] ys  zs
sum3' xs       []       zs       = sum3' xs  [0] zs
sum3' xs       ys       []       = sum3' xs  ys  [0]
sum3' (x : xs) (y : ys) (z : zs) = (x + y + z) : sum3' xs ys zs

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems xs = helper xs [] (head xs)
  where
    helper [] tmp prev = [tmp]
    helper (x : xs) tmp prev | x == prev = helper xs (x : tmp) x
                             | x /= prev = tmp : helper xs (x : []) x

{-
take n xs -> составляет подмассив из n элементов
drop n xs -> выкидывает из xs подмассив из n элементов
splitAt n xs -> разбивает список на два подсписка. Первый -> n элементов
xs !! n -> элемент n в списке
-}

take' :: Int -> [a] -> [a]
take' n _ | n <= 0 = []
take' _ []         = []
take' n (x : xs)   = x : take' (pred n) xs

drop' :: Int -> [a] -> [a]
drop' n xs | n <= 0 = xs
drop' _ []          = []
drop' n (_ : xs)    = drop' (pred n) xs

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs | n <= 0 = ([], xs)
splitAt' _ []          = ([], [])
splitAt' n (x : xs)    = (x : as, bs) where (as, bs) = splitAt' (pred n) xs

splitAt'' :: Int -> [a] -> ([a], [a])
splitAt'' n xs = (take' n xs, drop' n xs)

(!!!) :: [a] -> Int -> a
xs       !!! n | n < 0 = error "Index must be positive"
[]       !!! _         = error "Index too large"
(x : _)  !!! 0         = x
(_ : xs) !!! n         = xs !!! (pred n)


{-
filter predicate xs фильтрует список по предикату
takeWhile predicate xs берет элементы списка по одному из головы до тех пор, пока для них выполняется предикат
dropWhile predicate xs имеет обратный эффект функции takeWhile
span predicate xs возвращает пару списков (takeWhile predicate xs, dropWhile predicate xs)
break predicate xs инвертирует предикат
-}

filter' :: (t -> Bool) -> [t] -> [t]
filter' p [] = []
filter' p (x : xs)
  | p x       = x : filter' p xs
  | otherwise =  filter' p xs

takeWhile' :: (t -> Bool) -> [t] -> [t]
takeWhile' p [] = []
takeWhile' p (x : xs)
  | p x       = x : takeWhile' p xs
  | otherwise = []

{- @ это синтаксическая конструкция, именуемая синоним -}

dropWhile' :: (t -> Bool) -> [t] -> [t]
dropWhile' p [] = []
dropWhile' p xs@(x : xs')
  | p x = dropWhile' p xs'
  | otherwise = xs

span' :: (t -> Bool) -> [t] -> ([t], [t])
span' p xs = (takeWhile' p xs, dropWhile' p xs)

break' :: (t -> Bool) -> [t] -> ([t], [t])
break' p = span (not . p)

qsort :: Ord a => [a] -> [a]
qsort []            = []
qsort [x]           = [x]
qsort xs@(x : xs')  = qsort (filter (< x) xs) ++ x : qsort (filter (>= x) xs')

{-
map f xs применяет f к xs
concat [[xs]] конкатенирует списки в один
concatMap f xs применяет f к xs, а затем конкатенирует полученные списки
-}

map' :: (a -> b) -> [a] -> [b]
map' f []       = []
map' f (x : xs) = f x : map' f xs

concat' :: [[a]] -> [a]
concat' []         = []
concat' (xs : xss) = xs ++ concat' xss

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f = concat' . map' f

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x^2, x^3])

{-perms :: [a] -> [[a]]
perms []       = []
perms (x : xs) = temp [] x xs []

temp _  x []           acc = x : acc
temp pr x cu@(nx : nu) acc = temp (x : pr) nx nu (x : acc)

map (e :) (список списков) -}

perms' :: Eq a => [a] -> [[a]]
perms' []  = [[]]
perms' [e] = [[e]]
perms' es = concatMap (\e -> map (e :) (perms' (filter (/= e) es))) es

perms :: [a] -> [[a]]
perms [] = [[]]
perms es = solver [] es [] (length es)
  where
    solver _    [e]    _   1   = [[e]]
    solver _    []     acc _   = acc
    solver rest (e:es) acc len = solver (e:rest) es (acc ++ map (e :) (solver [] (rest++es) [] (pred len))) len

-- Требует более детального понимания
permsOpt :: [a] -> [[a]]
permsOpt []     = [[]]
permsOpt [e]    = [[e]]
permsOpt (e:es) = concatMap (insertElement e) (permsOpt es)
  where
    insertElement e []         = [[e]]
    insertElement e xss@(x:xs) = (e : xss) : map (x :) (insertElement e xs)


{-
and :: [Bool] -> Bool возвращает True, если все элементы списка True
or :: [Bool] -> Bool возвращает True, если хотя бы один элемент списка True
all :: (a -> Bool) -> [a] -> Bool возвращает True, если все элементы списка удовлетворяют предикату
any :: (a -> Bool) -> [a] -> Bool возвращает True, если хотя бы один элемент списка удовлетворяет предикату
-}

all' :: (a -> Bool) -> [a] -> Bool
all' predicate = and . map predicate

{-
words :: String -> [String] принимает строку, а возвращает массив слов в этой строке (сплит через пробел)
unwords :: [String] -> String это words наоборот
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-}

delAllUpper :: String -> String
delAllUpper = unwords . filter (not . all isUpper) . words

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

{-
  Функция, которая выводит какой-то результат через частичные промежутки времени и при этом продолжает
свое выполнение называется продуктивной.
-}

nats n = n : nats (n+1)

tenFirstNats = take 10 $ nats 1
headOfNats = head $ nats 100

{-
head :: [x] -> x
head []     = error "Where is the head Lebowski?!"
head (x:xs) = x

head $ nats 100                   -- Сопоставляем с образцом в head. Это нас обязывает сделать вызов nats
~> head (100 : nats (100 + 1))    -- Образец сопоставлен. Подставляем определение head
~> 100

  На втором этапе происходит приведение аргумента head к одному из образцов, затем x связывается с 100,
а отложенное вычисление nats (100 + 1) связывается с xs. Затем происходит подстановка определения функции
для соответствующего образца. Вот еще один пример:
head $ nats (98 + 2)
~> head ((98 + 2) : nats ((98 + 2) + 1))
~> 98 + 2
~> 100
-}

{-
ВАЖНО:
  Когда вызывается функция, у которой в определении есть сопоставление с образцом, то
ее аргумент должен быть приведен к виду какого-либо из образцов. Т.е. если аргументом является
вызов какой-либо функции, то будет сперва произведен вызов этой функции!
-}

{-
  Функции типа take, map, etc настолько ленивые, насколько это возможно и поэтому с их помощью можно
работать с бесконечными списками
-}

{-
repeate :: a -> [a] бесконечный список повторяющегося значения
replicate :: Int -> a -> [a] конечный список повторяющегося значения
cycle :: [a] -> [a] конкатенирует список бесконечное число раз
iterate :: (a -> a) -> a -> [a] применяет функцию к каждому последующему элементу, генерируя
    тем самым последовательность
-}

{---------------------------------------------------}

{- Арифметические последовательности -}

{-
[1..10] - то же самое, что и функция enumFromTo:
enumFromTo :: a -> a -> [a] генерирует список от левой границы до правой

шаг задается с помощью передачи в конструктор двух элементов
[1,2..9] - то же самое, что и функция enumFromThenTo
enumFromThenTo :: a -> a -> a -> [a]

[1..] - также допустимо (то же самое, что enumFrom)
[1,2..] - также допустимо (то же самое, что и enumFromThen)
-}

data Odd = Odd Integer 
  deriving (Eq, Show)

{- My solution
instance Enum Odd where
  succ (Odd n) = Odd $ succ . succ $ n
  pred (Odd n) = Odd $ pred . pred $ n
  toEnum idx = Odd $ toInteger $ idx `div` 2
  fromEnum (Odd n) = fromEnum n `div` 2
  enumFrom n = n : enumFrom (succ n)
  enumFromThen a b = a : b : solver b 1
    where
      step = fromEnum b - fromEnum a
      absStep = abs step
      inc | step > 0  = succ
          | step == 0 = id
          | step < 0  = pred
      solver n idx | idx == absStep = (inc n) : solver (inc n) 1
                   | otherwise   = solver (inc n) (succ idx)
  enumFromTo a b | num > 0 = take num $ enumFrom a
                 | otherwise = []
    where
      num = fromEnum b - fromEnum a + 1
  enumFromThenTo a b c | num > 0 = take num $ enumFromThen a b
                       | otherwise = []
    where
      step = abs $ fromEnum b - fromEnum a
      num = (abs $ fromEnum c - fromEnum a) `div` step + 1
-}

instance Enum Odd where
  toEnum i = Odd(toInteger i)
  fromEnum (Odd n) = fromEnum n

  succ (Odd n) = Odd (n+2)
  pred (Odd n) = Odd (n-2)

  enumFrom (Odd n) = map Odd [n,n+2..]
  enumFromTo (Odd n) (Odd m) = map Odd [n,n+2..m]
  enumFromThen (Odd n) (Odd n') = map Odd [n,n'..]
  enumFromThenTo (Odd n) (Odd n') (Odd m) = map Odd [n,n'..m]

{-
  List comprehension

[x**2 | x <- [1..20]]
[x**2 | x <- [1..20], x**2 < 200]

[(x, y) | x <- [1, 2], y <- [1, 2]] получаем [(1,1),(1,2),(2,1),(2,2)]
правило перебора такое: чаще всего перебирается самый правый генератор
-}

-- Пифагоровы тройки:
xs = [1..200]
pythagorean_triple = [(a, b, c) | a <- xs, b <- xs, c <- xs, a^2 + b^2 == c^2, a <= b]

coins :: Num a => [a]
coins = [3, 25]

change :: (Ord a, Num a) => a -> [[a]]
change s | s < 0     = []
         | s == 0    = [[]]
         | otherwise = [c:cs | c <- coins, cs <- change $ s - c]

foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight f init []     = init
foldRight f init (x:xs) = x `f` foldRight f init xs
{-
foldr :: (a -> b -> b) -> b -> [a] -> b правая свертка
foldr f ini [1,2,3] ~> 
...
~> 1 `f` (2 `f` (3 `f` ini))

также эта функция как бы "пересобирает" список:
вместо [] подставляется ini
вместо : подставляется f
-}

foldLeft :: (b -> a -> b) -> b -> [a] -> b
foldLeft f init []     = init
foldLeft f init (x:xs) = foldLeft f (f init x) xs

{-
Аналогично левая свертка
foldl :: (b -> a -> b) -> b -> [a] -> b

foldl f ini [1,2,3] ~>
...
~> ((ini `f` 1) `f` 2) `f` 3
or
~> f (f (f ini 1) 2) 3

Однако foldl создает огромное отложенное вычисление. Не рекомендуется к использованию
foldl' форсирует вычисления. она збс
-}

meanList :: [Double] -> Double
meanList = (\(s, l) -> s/l) . foldr (\n (s, l) -> (s+n,l+1)) (0, 0)

evenOnly' :: [a] -> [a]
evenOnly' xs = fst $ foldr (\e (es, f) -> if f then (e : es, not f) else (es, not f)) ([], even $ length xs ) xs

evenOnly :: [a] -> [a]
evenOnly []         = []
evenOnly [_]        = []
evenOnly (x1:x2:xs) = x2: evenOnly xs

{-
foldl1 :: (a -> a -> a) -> [a] -> a
foldr1 :: (a -> a -> a) -> [a] -> a

принимают на вход список
правая свертка в качестве инициализирующего элемента использует последний элемент списка,
левая свертка - первый.
-}

{-
scanl :: (b -> a -> b) -> b -> [a] -> [b] то же, что и foldl, только сохраняет все промежуточные результаты
scanr :: (b -> a -> b) -> b -> [a] -> [b] то же, что и foldr, только сохраняет все промежуточные результаты

левое сканирование является продуктивной функцией
-}

scanLeft :: (b -> a -> b) -> b -> [a] -> [b]
scanLeft f init []     = [init]
scanLeft f init (x:xs) = init : scanLeft f (f init x) xs

factorials :: (Num a, Enum a) => [a]
factorials = scanl (*) 1 [1..]

{-
В модуле Data.List находится полезные методы:
find :: Foldable t => (a -> Bool) -> t a -> Maybe a  который находит первое вхождение элемента в список
lookup :: Eq a => a -> [(a, b)] -> Maybe b
unfoldr :: (b -> Maybe (a, b)) -> b -> [a] 
-}

unfoldRight :: (b -> Maybe (a, b)) -> b -> [a]
unfoldRight f ini = helper (f ini) where
  helper (Just (x, ini')) = x : unfoldRight f ini'
  helper Nothing          = []

{-
Оптимизатор Haskell умеет выкидывать все промежуточные списки, если к спику последовательно применяются
функции, основанные на fold и unfold
-}

revRange :: (Char,Char) -> [Char]
revRange = unfoldr (\(l, r) -> if l > r then Nothing else Just (r, (l, pred r)))