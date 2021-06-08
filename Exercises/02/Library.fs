module CS220.Library // This line declares Library module.

let prob1 n =
    let rec iter counter n harmonic =
        if counter > n then harmonic
        else iter (counter+1) n (harmonic + 1.0 / float counter)

    if n < 1 then nan
    else iter 1 n 0.0

let prob4 n =
    (** if n < 0 then -1
    elif n <= 1 then n
    else n * (n+1) / 2 **)
    let rec iter counter total =
        if counter > n then total
        else iter (counter+1) (total+counter)
    if n < 0 then -1 else iter 1 0

let prob5 m n =
    (** if n < 0 then -1
    else (2*m+n)*(n+1)/2 **)
    let rec iter counter total =
        if counter > n then total
        else iter (counter+1) (total + m + counter)
    if n < 0 then -1 else iter 0 0

let prob6 a b =
    let rec iter n N =
        if n > b then N
        elif (n%7=0) && (n%5<>0) then iter (n+1) (N+1)
        else iter (n+1) N
    if b < 0 || a > b then -1
    else iter a 0

let pow s n =
    let rec rev s idx reversedString =
        if idx = String.length s then reversedString
        else rev s (idx+1) (reversedString + string s.[String.length s - 1 - idx])
    let rec iter counter upper totalString s =
        if counter = upper then totalString
        else iter (counter + 1) upper (totalString + s) s
    if n < 0 then rev s 0 "" |> iter 0 -n "" else iter 0 n "" s

let smallestDivisor (n:uint32) =
    (** let rec iter (m:uint) =
        if n % m = 0u then m
        else iter (m+1u)
    if n = 1u || n = 0u then 0u else iter 2u **)
    let mCand = float n |> System.Math.Sqrt |> System.Math.Floor |> uint32
    let rec loop cur =
        if cur <= mCand then
            if n % cur = 0u then cur else loop (cur + 2u)
        else n
    if n = 1u || n = 0u then 0u elif n % 2u = 0u then 2u else loop 3u

let isPrime (n:uint32) =
    (** let smallestDiv = smallestDivisor n
    if smallestDiv = 0u || smallestDiv = n then true else false **)
    if n > 0u && smallestDivisor n = n then true else false