module Test where

{- module Test (EXPORT METHODS) where - директива экспорта. обеспечение инкапсуляции -}

{-
  Этапы компиляции модуля:
    1) Синтаксический разбор
    2) Проверка типов
    3) Перевод в более низкоуровневый язык (?)
    4) Оптимизация
    5) Кодогенерация
      5.1) Преобразование из низкоуровневого языка (?) в код SDG машины (машина, осуществляющая графовую
        редукцию нашей программы)
      5.2) Преобразование кода SDG машины в код языка C--
    6) Компиляция в код целевой платформы или код для LLVM
-}

import Data.Function
import Data.Char (toUpper, toUpper)
import Data.Char hiding (toTitle)
import qualified Data.Set as Set {- должны использоваться только полные имена с псевдонимом Set -}
{- явный импорт Prelude перекрывает все настройки, связанные с его импортом -}

{-
  Чистая функция - функция, значение которой полностью определяется значениями ее аргументов.
Единственный источник данных для функции - ее аргументы.
-}

{-
  Полное имя функции - это ее собственное имя и предшествующее имя модуля, в котором она объявляется.
-}

{-

acos (cos pi)
max 23 54
max (-23) 54

(max 23) 54

sumSquares x y = x**2 + y**2

rock'n'rofl = "Rock and rofl!"

sign x = if x >= 0 then (if x == 0 then 0 else 1) else (-1)

max5 = max 5
max5 10

max 6 7
6 `max` 7

6 + 7
(+) 6 7

[ ! # $ % & * + . / < = > ? @ \ ^ | - ~ ] = these are the operators we can override

infixl 6 *+*, ^+^

a ^+^ b = a^2 + b^2
(*+*) a b = a**2 + b**2

divideTwoBy = (/) 2
divTwoBy = (2 /)
divideByTwo = (/ 2)
minusThree = (- 3)
subtractThreeFrom (3 -)


priority decreasing (those lines are equals):
abs (max 5 (sin pi))
abs $ max 5 (sin pi)
abs $ max 5 $ sin pi

can be achieved with:
f $ x = f x

:type 'c'

Haskell number types:
Num:
	Int
	Integer
	Float
	Double

x = 15 :: Int

:t not gives us 	Bool -> Bool
:t (&&) gives us 	Bool -> Bool -> Bool	which is equivalent to Bool -> (Bool -> Bool)

discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum


At the beginning of the file after line `module ...`
import Data.Char
isDidgit 'x'


fst (2, True)
snd (3, False)

[1,2,3,4,5]


:info :
:info ++

factorial n = if n == 0 then 1 else factorial (n - 1)

factorial' 0 = 1
factorial' n = if n < 0 then error "n must be positive" else n * factorial' (n - 1)

undefined (to burn)

factorial'' n | n == 0 = 1
              | n > 0 = factorial'' (n - 1)
              | otherwise = error "n must be positive"

factHelper :: Integer -> Integer -> Integer
factHelper acc n | n == 0 = acc
                 | n > 0 = factHelper (n * acc) (n - 1)

fact :: Integer -> Integer
fact n | n >= 0 = factHelper 1 n
       | otherwise = error "n must be positive"


:set +s

-}


infixl 6 *+*, ^+^
(*+*) a b = a**2 + b**2
a ^+^ b = a^2 + b^2

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ (**) (fst p1 - fst p2) 2 + (**) (snd p1 - snd p2) 2

fibonacci :: Int -> Int
fibonacci n | n == 0 = 0
            | n == 1 || n == -1 = 1
            | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0 = fibonacci (n + 2) - fibonacci (n + 1)

factHelper :: Integer -> Integer -> Integer
factHelper acc n | n == 0 = acc
                 | n > 0 = factHelper (n * acc) (n - 1)

fact :: Integer -> Integer
fact n | n >= 0 = factHelper 1 n
       | otherwise = error "n must be positive"

someFunc :: Integer -> Integer
someFunc x | x > 0  = 1
           | x == 0 = 0
           | x < 0  = (-1)

linFibonacciHelper :: Integer -> Integer -> Integer -> Integer -> Integer
linFibonacciHelper sign prev acc n | abs n == 1 = acc
                                   | otherwise = linFibonacciHelper sign acc (prev + sign * acc) (n - 1)

linFibonacci :: Integer -> Integer
linFibonacci n | n == 0     = 0
               | abs n == 1 = 1
               | otherwise = linFibonacciHelper (signum n) 0 1 (abs n)

unreadableRoots :: Double -> Double -> Double
                -> (Double, Double)
unreadableRoots a b c =
  let {d = sqrt (b **2 - 4 * a * c); x1 = (-b - d) / (2 * a); x2 = (-b + d) / (2 * a)}
  in (x1, x2)

roots :: Double -> Double -> Double
      -> (Double, Double)
roots a b c = let
  d = sqrt (b **2 - 4 * a * c)
  x1 = (-b - d) / (2 * a)
  x2 = (-b + d) / (2 * a)
  in (x1, x2)


rootsDiff a b c = x1 - x2 where
  (x1, x2) = roots a b c

roots' :: Double -> Double -> Double
       -> (Double, Double)
roots' a b c = (x1, x2) where
  d = sqrt (b **2 - 4 * a * c)
  x1 = (-b - d) / (2 * a)
  x2 = (-b + d) / (2 * a)


linFibonacci' :: Integer -> Integer
linFibonacci' n = let
    solver sign prev acc n | n == 0     = prev
                           | abs n == 1 = acc
                           | otherwise  = solver sign acc (prev + sign * acc) (n - 1)
  in solver (signum n) 0 1 (abs n)

linFib :: Integer -> Integer
linFib n = solver (signum n) 0 1 (abs n) where
  solver sign prev acc n | n == 0     = prev
                         | abs n == 1 = acc
                         | otherwise  = solver sign acc (prev + sign * acc) (n - 1)



seqA :: Integer -> Integer
seqA n | n >= 0 = solver 1 2 3 n
       | otherwise = error "Parameter n must be positive."
  where
    solver a b c n | n == 0    = a
                   | n == 1    = b
                   | n == 2    = c
                   | otherwise = solver b c (c + b - 2 * a) (n - 1)

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count 0 = (0, 1)
sum'n'count x = solver 0 0 (abs x) where
  solver s n v | v == 0    = (s, n)
               | otherwise = solver (s + v `mod` 10) (n + 1) (v `div` 10)

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = solver 0 0 where
  grid = 100000
  step = (b - a) / grid
  solver res i | i == grid = step * res
               | i == 0    = solver ((f a + f b) / 2) (i + 1)
               | otherwise = solver (res + f (a + i * step)) (i + 1)

getSecondFrom :: t1 -> t2 -> t3 -> t2
getSecondFrom _ value _ = value

{- перемножает вторые элементы пар -}
multSecond = (*) `on` snd


simpleLambda = \x -> x**2
lenVec' x = \y -> sqrt $ x**2 + y**2
lenVec'' = \x -> \y -> sqrt $ x**2 + y**2
lenVec = \x y -> sqrt $ x**2 + y**2

{- складывает первые элементы двух пар пар ((a, b), (c, d)) -}
sumFirstFirst' = on (+) (\pp -> fst $ fst pp)

compose' f g = \x -> f $ g x
{- or we can use (.) compose operator -}
compose = \f g x -> (f.g) x

sumFirstFirst = on (+) (fst . fst)

{-
doIt x = f (g (h x))) = f ((g . h) x) = (f . g . h) x
-}
doIt = \f g h -> f . g . h

someTuple = (,) 4 True
anotherTuple = (,,,) "Hello" 100500 "my dear friend" True

nonCurryingFunction (a, b) = a^2 + b^2
curryingFunction a b = a^2 + b^2

curryingFunction' = curry nonCurryingFunction
nonCurryingFunction' = uncurry curryingFunction

myCurry f x y = f (x, y)
myUncurry f (x, y) = f x y

swap :: (a, b) -> (b, a)
swap = uncurry $ flip (,)

second :: (a, b) -> b
second = uncurry $ flip const

{- (Num Bool) -> имплементация класса типов Num в Bool позволит задать x
x :: (Num Bool) => Bool
x = True + False * False
-}

{- Классы типов -}
class MyTypeClass a where
  foo, bar :: a -> a -> Bool
  bar a b = not $ foo a b

instance MyTypeClass Int where
  foo a b = a == b

{- elem :: Eq a => a -> [a] -> Bool   проверяет, содержится ли элемент в списке -}

class Printable t where
  toString :: t -> String

instance Printable Bool where
  toString True = "true"
  toString False = "false"

instance Printable () where
  toString _ = "unit type"

instance (Printable t, Printable v) => Printable (t, v) where
  toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"

{-----------------}

class Equals t where
  eq, ne :: t -> t -> Bool
  a `eq` b = not $ a `ne` b
  a `ne` b = not $ a `eq` b

instance Equals Bool where
  True `eq` True = True
  False `eq` False = True
  _ `eq` _ = False

instance (Equals t, Equals v) => Equals (t, v) where
  a `eq` b = fst a `eq` fst b && snd a `eq` snd b


class (Equals t) => Ordering t where
  gt, lt, ge, le :: t -> t -> Bool

{--------------------}

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab x | doesEnrageGork x && doesEnrageMork x = (stomp . stab) x
                  | doesEnrageMork x                     = stomp x
                  | doesEnrageGork x                     = stab x
                  | otherwise                            = x


{- show / read - методы сериализации / десериализации -}
{-
  Если функция полиморфна по возвращаемому типу, ее вызов нужно типизировать:
    read "420" :: Integer
    read "[4, 2, 0]" :: [Double]
-}

{-
  Еще есть функция reads:
    reads "5 some string" :: [(Integer, String)]
-}

{-
  Enum определяет функции succ, pred для движения по перечислимому типу.
А также toEnum и fromEnum
-}

{--
  Класс типа Bounded определяет верхнюю и нижнюю границы типа через методы minBound и maxBound:
    minBound :: Int
-}

class (Bounded a, Enum a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x | x == maxBound = minBound
          | otherwise     = succ x

  spred :: a -> a
  spred x | x == minBound = maxBound
          | otherwise     = pred x

{-
  Класс типов Num:
  negate n - унарный минус
  abs n - модуль
  signum n - знак
  fromInteger x - очевидно

  При реализации класса типов необходимо соблюдать законы этих классов типов. Например для Num:
  LAW abs x * signum x == x

  Integral - класс типов целочисленного деления
  Fractional - класс типов деления с плавающей точкой
  Floating - класс типов стандартных математических функций
  RealFrac - класс типов, содержащий функции, связанные с округлением
  RealFloat - класс типов, реализующий внутри себя логику предстваления чисел с палваюещй точкой
-}

average :: Int -> Int -> Int -> Double
average a b c = (fromIntegral a + fromIntegral b + fromIntegral c) / 3

{-
  Модели вычисления:

  Редекс (redex) - Something to be reduced according to the rules of a formal system.

  В чистых функциональных языках, при усовии, что программа является завершающаяся,
результат не зависит от стратегии вычисления (она может быть как ленивой, так и энергичной).
В императивных языках подобное свойство не выполняется из-за наличия изменяемых переменных.

  Преимущества ленивой модели вычислений:
  1) Редундантные параметры функции не редуцируются, что может приводить к более оптимальной редукции:
    sumIt :: Int -> Int -> Int
    sumIt x y = x + y

    Ленивая модель вычисления:
    sumIt (2 + 3) 4
    ~> (2 + 3) + 4
    ~> 5 + 4
    ~> 9

    Энергичная модель вычисления:
    sumIt (2 + 3) 4
    ~> sumIt 5 4
    ~> 5 + 4
    ~> 9

  Недостатки ленивой модели вычислений:
  1) Полезные параметры функции не редуцируются, что может приводить к менее оптимальной редукции:
    dup :: Int -> (Int, Int)
    dup x = (x, x)

    Ленивая модель вычисления:
    dup (2 + 3)
    ~> (2 + 3, 2 + 3)
    ~> (5, 2 + 3)
    ~> (5, 5)

    Энергичная модель вычисления:
    dup (2 + 3)
    ~> dup 5
    ~> (5, 5)

  Общее заключение следующее:
  Если какие-то параметры игнорируются в правой части функции,
тогда преимущество имеет ленивая стратегия вычисления.
  Если какие-то параметры используются в правой части функции два или более раз,
тогда преимущество имеет энергичная модель вычисления.

  Механизм разделения:
  Заместо вычисления, вместо параметра подставляется умный указатель p, который указывает на область
памяти, где может храниться как отложенное вычисление (thunk), так и значение.
  Когда приходит время подстановки указателя, если он указывает на thunk, то происходит вычисление,
а затем и подстановка:

    dup (2 + 3)   { p = 2 + 3 }
    ~> (p, p)     { p = 5 }
    ~> (5, 5)

  Example:
    bar x y z = x + y
    foo a b = bar a a (a + b)
    value = foo (3 * 10) (5 - 2)

    foo (3 * 10) (5 - 2)
      ~> bar p p (p + (5 - 2))    p = 3 * 10
      ~> p + p
      ~> 30 + 30
      ~> 60


  Ленивая модель вычислений позволяет вызвать `const 24 undefined` без прерывания программы,
поскольку второй аргумент полностью игнорируется функцией и никогда не вызывается.

  В рамках ленивой модели мы можем элиминировать расходимости, используя такой подход.

  Нестрогая функция - функция, возвращающая не расходящиееся значение, если на вход ей было передано
расходящееся вычисление.
  Строгая функция - функция, возвращающая расходящиееся значение, если на вход ей было передано
расходящееся вычисление.

  В Haskell встроен специальный анализатор строгости, позволяющий оптимизировать программу. Если он видит,
что функция является строгой, он может заменить вычисления в рамках ленивой модели на вычисления в рамках
энергичной модели.

  Нормальная форма - форма выражения, при которой выражение не содержит редексов:
    NF:
      1) 42
      2) (15, True)
      3) \x -> x + 2
    Not NF:
      1) "Hello " ++ "world!"
      2) sin (pi / 2)
      3) (\x -> x + 2) 5
  Слабая головная нормальная форма (weak head normal form) - форма выражения, при которой выражение
находится в одном из следующих видов:
    1) Лямбда абстракция: \x -> x**2 + 2*3
    2) Конструктор данных (в т.ч. частично примененный): (3, 1 + 5) или (,) (4 * 5)
    3) Частично примененная встроенная функция: (+) (4**2)

  Отложенные вычисления могут сыграть злую шутку и переполнить нам всю ОЗУ к хренам. Чтобы форсировать
вычисления используется функция seq. Она форсирует вычисления своего первого аргумента и возвращает второй
в случае, если первый содящийся. Иначе результат функции расходящийся.
  seq :: a -> b -> b
  seq _|_ b = _|_
  seq a b = b

  seq форсирует вычисления первого аргумента до WHNF.

  Но seq не удобный. Юзаем $!
  ($!) :: (a -> b) -> a -> b
  f $! x = x `seq` f x

-}

factorial :: Integer -> Integer
factorial n | n >= 0    = helper 1 n
            | otherwise = error "Parameter n must be positive"
  where helper acc n | n == 0    = acc
                     | otherwise = (helper $! (acc * n)) (pred n)