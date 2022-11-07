module Nat where

import Prelude hiding ((+), (*), (^), (-), (/), pred, min, max, div)
    
data Nat = O | S Nat
    deriving (Eq , Show)

instance Ord Nat where
    (<=) O _ = True
    (<=) _ O = False
    (<=) (S n) (S m) =  n <= m

pred :: Nat -> Nat
pred O = O
pred (S n) = n

(+) :: Nat -> Nat -> Nat
n + O = n
n + S m = S (n + m)

(*) :: Nat -> Nat -> Nat
n * O = O
n * S m = n + (n * m)

(-) :: Nat -> Nat -> Nat
n - O = n
n - (S m) = pred(n - m)

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


