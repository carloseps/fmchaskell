{-# LANGUAGE GADTs, EmptyDataDecls #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use null" #-}
{-# HLINT ignore "Redundant if" #-}

module IRI where
  
import Prelude hiding (all, reverse, drop, take, last, init, dropWhile, takeWhile, gcd, (/), (-), max, min, (^), pred, (*), (+), elem)

data Nat where
    O :: Nat
    S :: Nat -> Nat
  deriving (Eq, Show) 

data Unit where
    Star :: Unit

data Empty

instance Ord Nat where
    (<=) O _ = True
    (<=) _ O = False
    (<=) (S n) (S m) =  n <= m

(+) :: Nat -> Nat -> Nat
n + O = n
n + S m = S (n + m)

(*) :: Nat -> Nat -> Nat
n * O = O
n * S m = n + (n * m)

pred :: Nat -> Nat
pred O = O
pred (S n) = n

(-) :: Nat -> Nat -> Nat
n - O = n
n - (S m) = pred(n - m)

(^) :: Nat -> Nat -> Nat
n ^ O = S O
n ^ S O = n
n ^ S m = n * (n ^ m)

fib :: Nat -> Nat
fib O = O
fib (S O) = S O
fib (S (S n)) = fib (S n) + fib n

fact :: Nat -> Nat
fact O = S O
fact (S n) = S n * fact n

dist :: Nat -> Nat -> Nat
dist n O = n
dist O n = n
dist (S n) (S m) = dist n m

min :: Nat -> Nat -> Nat
min n O = O
min O n = O
min (S n) (S m) = S (min n m)

max :: Nat -> Nat -> Nat
max O n = n
max n O = n
max (S n) (S m) = S (max n m)

-- usei o (/) pra representar o `quot´
(/) :: Nat -> Nat -> Nat
n / m
    | n < m = O
    | otherwise = S ((n - m) / m)

-- usei o (%) pra representar o `rem´
(%) :: Nat -> Nat -> Nat
n % m = n - ((n / m) * m)

div :: Nat -> Nat -> (Nat, Nat)
div n m = (n / m, n % m)

-- `mdc´
gcd :: Nat -> Nat -> Nat
gcd n O = n
gcd n m = gcd m (n % m)

-- `mmc´
lcm :: Nat -> Nat -> Nat
lcm n m = (n * m) / gcd n m

-- combinacao
comb :: Nat -> Nat -> Nat
comb _ O = O
comb (S n) (S m) = (fact n)/(fact m) * fact (n - m)

isEven :: Nat -> Bool
isEven O = True
isEven (S n) = isOdd n

isOdd :: Nat -> Bool
isOdd O = False
isOdd (S n) = isEven n

isZero :: Nat -> Bool
isZero n
    | n == O = True
    | otherwise = False

-- List funs --

data List a where
    Nil  :: List a
    Cons :: a -> List a -> List a
  deriving (Eq, Show) 

-- some functions of 2° Exam IRI are down below

append :: a -> [a] -> [a]
append w [] = [w]
append w (x : xs) = (x : append w xs)

-- loading...

-- insert :: Nat -> [Nat] -> [Nat]
-- insert _ [] = []
-- insert n (x : xs)
--     | S m == S O = 

atEvens :: [a] -> [a]
atEvens [] = []
atEvens [x] = [x]
atEvens (x : xs) = x : atOdds xs

atOdds :: [a] -> [a]
atOdds [] = []
atOdds [_] = []
atOdds (x : xs) = atEvens xs

anyEven :: [Nat] -> Bool
anyEven [] = True
anyEven (x : xs) = isEven x || anyEven xs

anyOdd :: [Nat] -> Bool
anyOdd [] = True
anyOdd (x : xs) = isOdd x || anyOdd xs

anyZero :: [Nat] -> Bool
anyZero [] = True
anyZero (x : xs) = isZero x || anyZero xs

any :: (Nat -> Bool) -> [Nat] -> Bool
any f [] = True
any f (x : xs) = f x || all f xs

--extintas pelo all!

-- allEven :: [Nat] -> Bool
-- allEven [] = True
-- allEven (x : xs) = isEven x && allEven xs

-- allOdd :: [Nat] -> Bool
-- allOdd [] = True
-- allOdd (x : xs) = isOdd x && allOdd xs

-- allZero :: [Nat] -> Bool
-- allZero [] = True
-- allZero (x : xs) = isZero x && allZero xs

all :: (Nat -> Bool) -> [Nat] -> Bool
all f [] = True
all f (x : xs) = f x && all f xs

tidy :: [Nat] -> Bool
tidy [] = True
tidy [x] = isEven x
tidy (x1 : x2 : xs) = isEven x1 && isOdd x2 && tidy xs

isSorted :: [Nat] -> Bool
isSorted [] = True --brigas sobre isso por favor
isSorted [x] = True --brigas sobre isso tbm por favor
isSorted (x : y : ys) = 
    if x <= y && isSorted (y : ys)
        then True
        else False

intersperse :: a -> [a] -> [a]
intersperse i [] = []
intersperse i [x] = [x]
intersperse i (x : xs) = x : i : intersperse i xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = append x (reverse xs)

removeAt :: Nat -> [Nat] -> [Nat]
removeAt _ [] = []
removeAt O (x : xs) = xs
removeAt (S n) (x : xs) = x : removeAt n xs

replaceAt :: Nat -> Nat -> [Nat] -> [Nat]
replaceAt w _ [] = []
replaceAt w O (x : xs) = (w : xs)
replaceAt w (S n) (x : xs) = x : replaceAt w n xs

addAt :: Nat -> Nat -> [Nat] -> [Nat]
addAt w _ [] = []
addAt w O (x : xs) = (x + w) : xs
addAt w (S n) (x : xs) = x : addAt w n xs

-- extintas pelo pw!

-- pwAdd :: [Nat] -> [Nat] -> [Nat]
-- pwAdd [] _ = []
-- pwAdd _ [] = []
-- pwAdd (x : xs) (y : ys) = ((x + y) : pwAdd xs ys)

-- pwMul :: [Nat] -> [Nat] -> [Nat]
-- pwMul [] _ = []
-- pwMul _ [] = []
-- pwMul (x : xs) (y : ys) = ((x * y) : pwMul xs ys)

pw :: (a -> b -> c) -> [a] -> [b] -> [c]
pw f [] _ = []
pw f _ [] = []
pw f (x : xs) (y : ys) = (f x y : pw f xs ys)

maxElem :: [Nat] -> Nat
maxElem [x] = x
maxElem (x : xs) = max x (maxElem xs)

minElem :: [Nat] -> Nat
minElem [x] = x
minElem (x : xs) = min x (minElem xs)

takeEvens :: [Nat] -> [Nat]
takeEvens [] = []
takeEvens (x : xs) = if isEven x then x:(takeEvens xs) else takeEvens xs

take :: Nat -> [Nat] -> [Nat]
take _ [] = []
take O _ = []
take (S n) (x : xs) = x : (take n xs)

drop :: Nat -> [Nat] -> [Nat]
drop _ [] = []
drop O xs = xs
drop (S n) (x : xs) = drop n xs 

head ::  List Nat -> Nat
head (Cons x _) = x

tail :: List Nat -> List Nat
tail (Cons _ xs) = xs

init :: List Nat -> List Nat
init (Cons _ Nil) = Nil
init (Cons x xs) = Cons x (init xs)

last :: List Nat -> Nat
last (Cons x Nil) = x
last (Cons _ xs) = last xs

predicate :: a -> Bool
predicate a = True

takeWhile :: (a -> Bool) -> List a -> List a
takeWhile predicate Nil = Nil
takeWhile predicate (Cons x xs) =
    if predicate x
     then Cons x (takeWhile predicate xs)
    else Nil

dropWhile :: (a -> Bool) -> List a -> List a
dropWhile predicate Nil = Nil
dropWhile predicate (Cons x xs) =
    if predicate x
        then dropWhile predicate xs
        else (Cons x xs)

elem :: Nat -> [Nat] -> Bool
elem _ [] = False
elem n (x : xs)
    | n == x = True
    | otherwise = elem n xs

--pick :: Nat -> List a -> Maybe a

