module DataTypes where

import Data.Function ((&), on)
import Data.List.Split (splitOn)
import Data.Char (isDigit)

import Prelude hiding (lookup)
import qualified Data.List as L


data B = T | F
    deriving (Show, Eq, Read, Enum)

{-
instance Show B where
    show T = "Not F"
    show F = "F"

or just using deriving syntax
-}

alwaysF :: a -> B
alwaysF x = F

-- Данный тип называется типом перечислений (типом сумм)
data Color = Red | Blue | Green
    deriving (Show, Eq, Ord, Read)

{-
Сопоставление с образцом происходит сверху вниз, слево направо
И имеет 3 варианта:
Удача
Неудача
Расходение
-}

data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Error = EQ
cmp Warning Warning = EQ
cmp Info Info = EQ
cmp Info _ = LT
cmp Error _ = GT
cmp Warning Info = GT
cmp Warning Error = LT


-- case of конструкция, сопоставляющая результат вызова некоторой функции с образцами

lessThanError :: LogLevel -> Bool
lessThanError logLevel = 
    case cmp logLevel Error of
        LT -> True
        _  -> False

-- Данный тип называется типом произведений (декартовых произведения соответствующих множеств)
data Point = Point Double Double

instance Show Point where
    show (Point x y) = show (x,y)

-- Данный тип назыается типом сумм произведений
data Roots = Roots Double Double | None

instance Show Roots where
    show (Roots x1 x2) = "Roots: " ++ show (x1, x2)
    show None          = "There are no roots"

roots :: Double -> Double -> Double -> Roots
roots a b c | d >= 0    = Roots x1 x2
            | otherwise = None
    where
        d = b ** 2 - 4 * a * c
        x1 = -b + sqrt d / (2 * a)
        x2 = -b - sqrt d / (2 * a)



data Shape = Circle Double | Rectangle Double Double deriving Show


square :: Double -> Shape
square a = Rectangle a a

{-
Однако, такая запись не пройдет:

isSquare :: Shape -> Bool
isSquare (square _) = True
isSquare _          = False

Такие функции используются в качестве публичного интерфейса нашего типа, скрывая детали реализации
конструкторов типа (Circle, Rectangle)
Примером служит модуль Data.Ratio:
Data.Ratio - модуль рациональных чисел. 
Позволяет конструировать рациональные дроби с помощью оператора (%)
-}

{-
Неопровержимых образцов бывает 3 типа:
1) Символ подчеркивания _
2) Имя переменной x
3) Ленивый образец ~... с которым ВСЕГДА сопоставление происходит удачно
его разбор происходит только в правой части, только когда нам понадобятся
входящие в данный образец переменные!
-}

fromMaybe (Just x) = x
fromMaybe Nothing  = error "!!!"

{-
fromMaybe' ~(Just x) = x
fromMaybe' Nothing  = error "!!!" -- сюда никогда не доберемся
-}

(***) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
(***) f g (x, y) = (f x, g y)

(****) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d) 
(****) f g ~(x, y) = (f x, g y)

{-
const 1 *** const 2 $ undefined - свалится
const 1 **** const 2 $ undefined - вернет  (1, 2)
-}

data Person' = Person' String String Int

firstName' :: Person' -> String
firstName' (Person' fn _ _) = fn

lastName' :: Person' -> String
lastName' (Person' _ ln _) = ln

age' :: Person' -> Int
age' (Person' _ _ age) = age

-- Но мы можем сделать гораздо проще
-- объявляя метки полей (проекции типа на поле)
data Person = Person { firstName :: String, lastName :: String, age :: Int }
    deriving (Show, Eq)

        
{-
So now we can do next:
den = Person "Denis" "Pribavkin" 22
firstName den

или можем воспользоваться оператором амперсанд из Data.Funciton:
infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x

den & firstName

При этом следует отметить, что & позволяет строить конструкции с точностью до наоборот,
создаваемые оператором $:
f :: Int -> String
g :: Bool -> Int

f $ g $ True
то же самое, что и
True & g & f
-}

{-
Также возможно следующее задание типов:
den = Person { age = 22, firstName = "Denis", lastName = "Pribavkin" }

При том некоторые поля можно не указывать
-}

-- С помощью синтаксиса записей можно обновлять поля записей. При этом будет создан
-- новый объект, т.к. типы в Haskell иммутабельны
updateAge :: Int -> Person -> Person
updateAge newAge person = person { age = newAge }

updateFirstAndLastNames :: String -> String -> Person -> Person
updateFirstAndLastNames fn ln person = person { firstName = fn, lastName = ln }

name'' :: Person -> String
name'' person = firstName person ++ " " ++ lastName person

name' :: Person -> String
name' (Person fn ln _)  = fn ++ " " ++ ln

name :: Person -> String
name (Person {firstName = fn, lastName = ln}) = fn ++ " " ++ ln

isRectangle :: Shape -> Bool
isRectangle Rectangle{} = True
isRectangle _ = False

abbrFirstName :: Person -> Person
abbrFirstName p@(Person {firstName = fn}) = if length fn < 2 then p else p {firstName = head fn : "."}

-- Параметризованные типы
data Coord t = Coord t t deriving Show

getCenter :: Double -> Coord Int -> Coord Double
getCenter len (Coord x y) = Coord (f $ fromIntegral x) (f $ fromIntegral y)
    where f x = (x * len) + len / 2

getCell :: Double -> Coord Double -> Coord Int
getCell len (Coord x y) = Coord (f x) (f y)
    where f x = ceiling (x / len) - 1

-- Haskell позволяет описывать конструкторы типов в префиксном стиле
-- даже в объявлении типа функции
twice :: a -> [] a
twice x = [x, x]

thrice :: a -> (,,) a a a
thrice x = (,,) x x x

-- То же самое касается функциональной стрелки ->
id' :: (->) a a
id' x = x

k :: a -> b -> a
k x y = x

k' :: (->) a ((->) b a)
k' x y = x

{-
Определения Maybe и Either:

data Maybe a = Nothing | Just a

data Either a b = Left a | Right b

В конструктор ДАННЫХ мы передаем ЗНАЧЕНИЯ
В конструктор ТИПА мы передаем ТИП

-}

maybeToList :: Maybe a -> [a]
maybeToList (Just x)  = [x]
maybeToList Nothing = []

listToMaybe :: [a] -> Maybe a
listToMaybe [x] = Just x
listToMaybe []  = Nothing


data Error = ParsingError | IncompleteDataError | IncorrectDataError String
    deriving Show

parsePerson :: String -> Either Error Person
parsePerson str = case validators of
    [True, True, True] -> Right $ Person firstName lastName $ read age
    [False,  _,     _] -> Left ParsingError
    [  _, False,    _] -> Left IncompleteDataError
    [  _,    _, False] -> Left $ IncorrectDataError age
    where
        parsedList = map (splitOn " = ") $ splitOn "\n" str
        parsingOk = all ((2 ==) . length) parsedList
        (completeData, firstName, lastName, age) = case parsedList of
            (["firstName", fn]:["lastName", ln]:["age", age]:_) -> (True, fn, ln, age)
            _                                                   -> (False, "", "", "")
        correctData = all isDigit age
        validators = [parsingOk, completeData, correctData]


{-
:kind служит для чека типов
Все базовые типы без параметров имеют kind *
:kind Int вернет Int :: *
:kind Mayby вернет Maybe :: * -> *
-}

{-
Флаг строгости ! , который помещается на элементы типа при его объявлении форсирует вычисления
при создании объекта
-}

data CoordLazy t = CoordLazy t t deriving Show
data CoordStrict t = CoordStrict !t !t deriving Show

{-

Инфиксный конструктор данных позволяет создавать такие типы данных

import Data.Complex
import Data.Ratio

data Complex a = !a :+ !a
data Ratio a = !a :% !a
-}

--Конструктор данных ничем не отличается от функции и поэтому мы имеем право делать так:
coords = 3 `CoordLazy` 4

{-
Вывод - конструкторы типов данных можно задавать в виде операторов. 
В Haskell, однако, есть договорённость - любой инфиксный конструктор данных должен начинаться
с двоеточия :
Это некоторый эквивалент большой буквы, принятой для отличия конструкторов типов данных от функций
-}

-- Рекурсивные типы данных. Пишем свой список:
infixr 5 :* 
data List a = Nil | a :* (List a) deriving Show

-- Бинарное дерево
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

height :: Tree a -> Int
height tree = helper 0 tree
    where
        helper n (Leaf _)          = n
        helper n (Node left right) = on max (helper (succ n)) left right

size :: Tree a -> Int
size (Leaf _)          = 1
size (Node left right) = 1 + size left + size right

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf e)          = (1, e)
    go (Node left right) = (n1 + n2, e1 + e2) where
        (n1, e1) = go left
        (n2, e2) = go right

-- Тип данных арифметических выражений
infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand e = helper e $ e :+: Val 0
    where
        helper current previous | current == previous = previous
                                | otherwise           = helper (solve current) current
        
        solve ((e1 :+: e2) :*: e) = solve e1 :*: solve e :+: solve e2 :*: solve e
        solve (e :*: (e1 :+: e2)) = solve e1 :*: solve e :+: solve e2 :*: solve e
        solve (e1 :*: e2) = solve e1 :*: solve e2
        solve (e1 :+: e2) = solve e1 :+: solve e2
        solve e = e

-- Синонимы типов

-- type String = [Char]

type IntegerList = [Integer]

sumSquares :: IntegerList -> Integer
sumSquares = foldl1 (+) . map (^2)

-- Все интерфейсы, определенные для данного типа, определены и для его синонима

-- Синонимы типов с параметром

type AssocList k v = [(k, v)]

lookup' :: Eq k => k -> AssocList k v -> Maybe v
lookup' _ []             = Nothing
lookup' k ((k', v) : vs) | k == k'   = Just v
                         | otherwise = lookup' k vs

-- Можно использовать частичное применение типа с параметром
-- Для чека типов можно использовать :kind

type IntAssocList v = AssocList Int v

type Endo a = a -> a

foo :: Endo (Endo a)
foo f = f

-- Обертки типов
-- Объявляя обертку типов, все интерфейсы, определенные для обертываемого типа, пропадают
newtype IntList = IntList [Int] deriving Show
example = IntList [1..10]

{-
Отличия newtype от data:
1) Тип, объявленный как newtype имеет ровно один конструктор с ровно одним параметром
2) Тип, объявленный как newtype, более ленив
-}

data IntList' = IntList' [Int] deriving Show

ignore :: IntList -> String
ignore (IntList _) = "Hello World!"

ignore' :: IntList' -> String
ignore' (IntList' _) = "Hello World!"


{-
Теперь, вызвав 
ignore $ IntList undefined
и
ignore' $ IntList' undefined
только во втором случае получим ошибку

более того, аналогичный результат можно получить, вызвав
ignore udefined
и
ignore' undefined
-}

newtype Identity a = Identity {runIdentity :: a} deriving (Eq, Ord, Show)

{-
Еще один забавный момент заключается в том, что во время выполнения программы все упоминания
типа, определенного как newtype, стираются и по сути Haskell работает непосредственно с
обертываемым типом.
-}

{-
    Моноид - это некоторое множество с заданной на нём бинарной ассоциативной операцией.
Также в данном множестве должен находиться нейтральный элемент относительно этой операции.

Класс типов Monoid описывает именно такое поведение:

class Monoid a where
    mempty :: a             -- нейтральный элемент
    mappend :: a -> a -> a  -- бинарная ассоциативная операция

    mconcat :: [a] -> a
    mconcat = foldr mappend mempty  -- свертка

Законы для класса типов Monoid:
1)  mempty `mappend` x == x
2)  x `mappend` mempty == x
3)  (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
-}

{-
Если мы хотим упаковать числа в моноид, то у нас существуют 2 бинарные ассоциативные операции:
* и +
Шоделать.
Берем и пихаем числа в newtype

P.S.
    В новых версиях Haskell mappend - это всего лишь синоним для операции (<>),
которая переехала в класс типов Semigroup
-}

newtype Sum a = Sum { getSum :: a }
    deriving (Show, Eq, Ord, Read, Bounded)

instance Num a => Semigroup (Sum a) where
    (Sum x) <> (Sum y) = Sum (x + y)

instance Num a => Monoid (Sum a) where
    mempty = Sum 0

-- ------------------------------------
newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Semigroup Xor where
    (Xor x) <> (Xor y) = Xor (x /= y)

instance Monoid Xor where
    mempty = Xor False

-- ------------------------------------
newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Monoid a => Semigroup (Maybe' a) where
    (Maybe' (Just left)) <> (Maybe' (Just right)) = Maybe' $ Just $ left <> right
    _                <>  _                        = Maybe' Nothing

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' $ Just mempty

-- -----------------------------------------------

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

instanceh











