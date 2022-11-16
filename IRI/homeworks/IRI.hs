{-# LANGUAGE GADTs, EmptyDataDecls #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module IRI where
  
import Prelude hiding (last, init, dropWhile, takeWhile, gcd, (/), (-), max, min, (^), pred, (*), (+))

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

isEven :: Nat -> Bool
isEven O = True
isEven (S n) = isOdd n

isOdd :: Nat -> Bool
isOdd O = False
isOdd (S n) = isEven n

-- List funs --

data List a where
    Nil  :: List a
    Cons :: a -> List a -> List a
  deriving (Eq, Show) 

-- Functions of 2° Exam IRI

atEvens :: [a] -> [a]
atEvens [] = []
atEvens [x] = [x]
atEvens (x : xs) = x : atOdds xs

atOdds :: [a] -> [a]
atOdds [] = []
atOdds [x] = [x]
atOdds (x : xs) = atEvens xs

takeEvens :: [Nat] -> [Nat]
takeEvens [] = []
takeEvens (x : xs) = if isEven x then x:(takeEvens xs) else takeEvens xs

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

--pick :: Nat -> List a -> Maybe a
