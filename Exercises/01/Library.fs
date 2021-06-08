module CS220.Library // This line declares Library module.

let prob1 a b c =
    let square x = uint64 x * uint64 x
    let checkOF x = x <> uint64 (int x)
    let a1 = max a b |> square
    let a2 = min a b |> max c |> square
    if checkOF a1 || checkOF a2 || checkOF (a1 + a2) then -1
    else a1 + a2 |> int

let prob2 s =
    let length = String.length s
    if (length = 0) || (s.[length-1] <> '\n') then s + "\n"
    else s

let prob3 a b c =
    let delta = b * b - 4.0 * a * c
    if delta < 0.0 then nan
    else (-b + sqrt delta) / (2.0*a)

let prob4 month =
    (* let month_days = [31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31]
    if m > 12 || m < 1 then -1
    else month_days.Item(m-1) *)
    if month = 1 || month = 3 || month = 5 || month = 7 ||
        month = 8 || month = 10 || month = 12 then 31
    elif month = 2 then 28
    elif month = 4 || month = 6 || month = 9 || month = 11 then 30
    else -1

let prob5 year month =
    let isLeapYear year =
        if year % 4 = 0 then
            if year % 100 = 0 then
                if year % 400 = 0 then true
                else false
            else true
        else false

    if year < 0 then -1
    elif month = 1 || month = 3 || month = 5 || month = 7 ||
        month = 8 || month = 10 || month = 12 then 31
    elif month = 2 then if isLeapYear year then 29 else 28
    elif month = 4 || month = 6 || month = 9 || month = 11 then 30
    else -1