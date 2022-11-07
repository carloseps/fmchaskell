module Nat where

import Prelude hiding ((+), (*), (^), (-), pred, min, max)
    
data Nat = O | S Nat
    deriving (Eq , Show)

(+) :: Nat -> Nat -> Nat
n + O = n
n + S m = S (n + m)

(*) :: Nat -> Nat -> Nat
n * O = O
n * S m = n + (n * m)

(-) :: Nat -> Nat -> Nat
m - O = m
n - (S m) = pred(n - m)

pred :: Nat -> Nat
pred O = O
pred (S n) = pred n

fib :: Nat -> Nat
fib O = O
fib (S O) = S O
fib (S (S n)) = fib (S n) + fib n

(^) :: Nat -> Nat -> Nat
n ^ O = S O
n ^ S O = n
n ^ S m = n * (n ^ m)

fact :: Nat -> Nat
fact O = S O
fact (S n) = S n * fact n

dist :: Nat -> Nat -> Nat
dist n O = O
dist O (S n) = S n
dist (S n) (S m) = dist n m

min :: Nat -> Nat -> Nat
min O n = O
min n O = n
min (S n) (S m) = S (min (n) (m))

max :: Nat -> Nat -> Nat
max O n = n
max n O = n
max (S n) (S m) = S (max (n) (m))


