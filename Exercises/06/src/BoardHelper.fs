module CS220.BoardHelper

let checkWinner (states: SlotState []) : Marker option =
    let checkWinSequence (states: SlotState []) fstIdx sndIdx thrIdx : Marker option =
        match states.[fstIdx], states.[sndIdx], states.[thrIdx] with
        | Marked (first), Marked (second), Marked (third) -> if first = second && second = third then Some first else None
        | _ -> None

    // generate winning combinations
    let leftDiagonalWinner = checkWinSequence states 0 4 8
    let rightDiagonalWinner = checkWinSequence states 2 4 6
    let rowWinners = List.fold (fun acc row -> (checkWinSequence states (row*3) (row*3+1) (row*3+2))::acc) [] [0..2]
    let colWinners = List.fold (fun acc col -> (checkWinSequence states (col) (col+3) (col+6))::acc) [] [0..2]
    let winnersList = [leftDiagonalWinner; rightDiagonalWinner] @ rowWinners @ colWinners

    let isWinner = function
        | None -> false
        | _ -> true
    match List.tryFind isWinner winnersList with
    | None -> None
    | Some (winner) -> winner

let isDraw (states: SlotState []) : bool =
  let folder acc = function
    | EmptySlot -> acc + 1
    | _ -> acc
  let numberOfEmptySlots = Array.fold folder 0 states
  if numberOfEmptySlots <> 0 then false
  else
    match checkWinner states with
    | None -> true
    | _ -> false