module CS220.Library // This line declares Library module.

let countLetter s1 s2 =
    let rec loop acc currentIdx (s1:string) (s2:string) =
        if currentIdx = String.length s1 then acc
        elif s1.[currentIdx] = s2.[0] then loop (acc+1UL) (currentIdx+1) s1 s2
        else loop acc (currentIdx+1) s1 s2
    if s1 = "" || s2 = "" then 0UL else loop 0UL 0 s1 s2

let diagonal matrix =
    let folder (diag, cnt) (row: 'a list) =
        row.[cnt]::diag, (cnt+1)
    if matrix = [[]] then []
    else List.fold folder ([], 0) matrix |> fst |> List.rev

let transpose matrix =
    let folder1 idx acc (row: 'a list) =
        row.[idx]::acc
    let folder2 acc idx =
        (List.fold (folder1 idx) [] matrix) :: acc
    List.fold folder2 [] [0 .. List.length matrix - 1]
    |> List.map List.rev |> List.rev

let rotate list n =
    let getListBeforeOrStartingAt k compare =
        let folder (resultingList, acc) element =
            if compare acc k then element::resultingList, (acc+1)
            else resultingList, (acc+1)
        List.fold folder ([], 0) list |> fst |> List.rev

    let length = List.length list
    let offset = abs n % length
    if offset = 0 then list
    else
        let endPosition = if n > 0 then length - offset else offset
        getListBeforeOrStartingAt endPosition (>=) @ getListBeforeOrStartingAt endPosition (<)

let rec hanoi a b n =
    if n = 1 then [(a, b)]
    else
        let c = 6 - a - b
        hanoi a c (n-1) @ hanoi a b 1 @ hanoi c b (n-1)