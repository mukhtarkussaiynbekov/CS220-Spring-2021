# Set 02

Only selected problems will have a solution.

### Problem 1

Write a function `prob1` that takes in a number `n` (int) and returns the n-th
[harmonic number](https://en.wikipedia.org/wiki/Harmonic_number) (float). For
any error case, this function should return `nan`.

```fsharp
val prob1: int -> float
```

##### Solution

```fsharp
let prob1 n =
  let rec aux ans n =
    if n = 1 then ans else aux (ans + 1.0 / float n) (n - 1)
  if n < 1 then nan else aux 1.0 n
```

### Problem 2

How many different ways can we make change of a given amount of money in Korean
coins? Suppose we have 5 different kinds of coins: 500-won coins, 100-won coins,
50-won coins, 10-won coins, and 1-won coins. Write a function `prob2` that takes
in an amount of money in won, and returns the number of possible
combinations. This function should return `-1` if an errorneous input is given,
e.g., when negative amount is given.

```fsharp
val prob2: int -> int
```

### Problem 3

Write a function `prob3` that computes GCD (Greatest Common Divisor) of two
given integers. This function should return `-1` for any error cases.

```fsharp
val prob3: int -> int -> int
```

### Problem 4

Write a function `prob4` that takes in an integer `n` and returns an integer
`1 + 2 + ... + (n-1) + n`. If n is a negative integer, this function should
return `-1`. This function should return `-1` for any error cases.

```fsharp
val prob4: int -> int
```

### Problem 5

Write a function `prob5` that takes in two integers `m` and `n`, and returns an
integer `m + (m+1) + (m+2) + ... + (m + (n-1)) + (m + n)`. This function should
return `-1` for any error cases. For instance, if `n` is negative, the function
should return `-1`.

```fsharp
val prob5: int -> int -> int
```

##### Solution

```fsharp
let prob5 m n =
  let rec aux ans n =
    if n < 0 then ans else aux (ans + m + n) (n - 1)
  if n < 0 then -1 else aux 0 n
```

### Problem 6

Write a function `prob6` that takes in two positive integers `a` and `b`, and
returns `N` where `N` is the number of possible `n` such that satisfies the
following three conditions: (1) `a <= n <= b`; (2) `n` is divisible by 7; and
(3) `n` is not a multiple of `5`. Always return `-1` for all error cases, such
as when `a > b` or when `b < 0`.

### Problem 7

Declare a function `pow` that takes in a string `s` and an integer `n`, and
returns a string that repeats `s` for `n` times. For example, `pow "abc" 3`
should return "abcabcabc". When `n` is zero, the function returns an empty
string, and when `n` is negative, it returns a string that repeats reversed `s`
for `-n` times. For example, `pow "abc" -3` will return "cbacbacba".

##### Solution

```fsharp
let pow s n =
  let rec loop i acc s = if i = 0 then acc else loop (i - 1) (s + acc) s
  if n < 0 then rev s 0 "" |> loop -n "" else loop n "" s
```

### Problem 8

Write a function `smallestDivisor` that takes in an unsigned integer `n`, and
returns the smallest integral divisor of `n` that is greater than 1. For
example, given 45, the function will return 3 (`45 % 3 = 0`). This function
returns `0` for all error cases, such as when the given number is 1.

##### Solution

```fsharp
let smallestDivisor (n: uint32) =
  let mCand = float n |> System.Math.Sqrt |> System.Math.Floor |> uint32
  let rec loop cur =
    if cur <= mCand then
      if n % cur = 0u then cur else loop (cur + 2u)
    else n
  if n = 1u || n = 0u then 0u elif n % 2u = 0u then 2u else loop 3u
```

### Problem 9

Write a function `isPrime` that takes in an unsigned integer, and check if it is
a prime number or not. If the number is prime, then the function returns true.
Otherwise, it returns false. Hint: use the `smallestDivisor` function of problem
3.

##### Solution

```fsharp
let isPrime n =
  if n > 0u && smallestDivisor (uint32 n) = uint32 n then true else false
```

### Problem 10

Write a function `isFeasible` that takes in three unsigned integers `a`, `b`,
and `c`, and returns boolean indicating whether one can obtain `c` by combining
all the unsigned integers from `a` to `b` with either plus or minus operator.
Specifically, for all unsigned integers `a <= n <= b`, we concatenate them in
the order using an operator OP: `(a) OP (a+1) OP (a+2) OP ... OP (b-1) OP (b)`,
where OP can be either `+` or `-`. For example, `isFeasible 1 3 0` returns
`true`, because we can obtain 0 by `1 + 2 - 3 = 0`. However, `isFeasible 1 3 4`
returns `false`, because we cannot obtain 4 with any combination. For all error
cases, this function returns false. For example, when `a > b`, the function
simply returns false.
