module CS220.Library // This line declares Library module.

let rev lst =
    let rec loop acc = function
        | [] -> acc
        | h::t -> loop (h::acc) t
    loop [] lst

let removeOdd lst =
    (**let rec loop acc = function
        | h::t when h%2=0 -> loop (h::acc) t
        | h::t -> loop acc t
        | _ -> acc
    loop [] lst |> rev**)
    let rec loop = function
        | [] -> []
        | h::t when h%2=0 -> h :: loop t
        | h::t -> loop t
    loop lst

let getSmallest lst =
    (**let rec loop currentSmallest = function
        | [] -> currentSmallest
        | h :: t -> loop (min currentSmallest h) t**)

    let rec loop currentSmallest = function
        | [] -> currentSmallest
        | h::t when h < currentSmallest -> loop h t
        | h::t -> loop currentSmallest t

    match lst with
        | [] -> None
        | h :: t -> loop h t |> Some

let len lst =
    let rec loop acc = function
        | [] -> acc
        | h::t -> loop (acc+1u) t
    loop 0u lst

let take lst (n:uint32) =
    let rec loop cutline curLength acc = function
        | [] -> acc
        | h::t when curLength <= cutline -> loop cutline (curLength-1u) (h::acc) t
        | h::t -> loop cutline (curLength-1u) acc t

    let length = len lst
    if n >= length then lst else rev lst |> loop n length []

let rec contains n = function
    | h::t when n = h -> true
    | h::t -> contains n t
    | _ -> false

let count n lst =
    let rec loop acc target = function
        | h::t when h = target -> loop (acc+1) target t
        | h::t -> loop acc target t
        | _ -> acc
    loop 0 n lst

let runLength lst =
    let rec loop seen acc = function
        | h::t when contains h seen -> loop seen acc t
        | h::t -> loop (h::seen) ((h, count h lst)::acc) t
        | _ -> acc
    loop [] [] lst |> rev

let isPalindrome lst =
    let rec loop l1 l2 =
        match l1, l2 with
        | h1::t1, h2::t2 when h1 = h2 -> loop t1 t2
        | [], [] -> true
        | _ -> false
    rev lst |> loop lst

let slice lst a b =
    let rec loop a b curLength acc = function
        | [] -> acc
        | h::t when curLength >= a && curLength <= b -> loop a b (curLength-1) (h::acc) t
        | h::t -> loop a b (curLength-1) acc t

    let length = int (len lst)
    if a < b then rev lst |> loop a b length [] else rev lst |> loop b a length []