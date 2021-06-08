module CS220.Library // This line declares Library module.

let pair (x: int) (y: int) m : int = m x y

let fst pair = pair (fun x y -> x)

let isEarly t1 t2 =
    match t1.AMPM, t2.AMPM with
    | AM, PM -> true
    | PM, AM -> false
    | _ ->
        if t1.Hours < t2.Hours then true
        elif t1.Hours > t2.Hours then false
        elif t1.Minutes < t2.Minutes then true
        else false

(**let addMinutes t m =
    let addHours = (m / 60) % 24
    let minutesAbsolute = (abs m) % 60
    let addMinutes = if m >= 0 then minutesAbsolute else -1 * minutesAbsolute
    if t.Minutes + addMinutes < 0 then
        let newMinutes = t.Minutes + addMinutes + 60
        if t.Hours + addHours - 1 <= 0 then
            let newHours = t.Hours + addHours - 1 + 24
            if t.Hours + addHours - 1 > -12 then {t with Hours = newHours; Minutes = newMinutes}
            else
                match t.AMPM with
                | AM -> {Hours = newHours; Minutes = newMinutes; AMPM = PM}
                | PM -> {Hours = newHours; Minutes = newMinutes; AMPM = AM}
        else
            let newHours = t.Hours + addHours - 1
            {t with Hours = newHours; Minutes = newMinutes}
    elif t.Minutes + addMinutes >= 60 then
        let newMinutes = t.Minutes + addMinutes - 60
        if t.Hours + addHours + 1 > 12 then
            if t.Hours + addHours + 1 >= 24 then {t with Hours = t.Hours + addHours + 1 - 24; Minutes = newMinutes}
            else
                match t.AMPM**)

(**let mirrorX = function
    | StraightLine (a, b) -> StraightLine (-a, -b)

let mirrorY = function
    | StraightLine (a, b) -> StraightLine (-a, b)**)