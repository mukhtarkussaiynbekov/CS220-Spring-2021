namespace CS220

/// Minimax strategy.
type MinimaxStrategy () =
  inherit AI ()
    override __.NextMove (player: Marker) (board:Board) =
      let slots = [1 .. 9]

      let areMovesLeft (board:Board) =
        List.fold (fun acc slot -> acc || not (board.IsOccupied slot)) false slots

      let boardScore (player: Marker) (board:Board) =
        match board.CheckWinner () with
        | Some (winner) -> if player = winner then 10 else -10
        | _ -> 0

      let markSlot (board:Board) slot marker =
        let newBoard = board.Copy ()
        newBoard.Mark slot marker |> ignore
        newBoard

      let rec minimax (player: Marker) (board:Board) depth isMax =
        let minimaxFolder comparator acc slot =
            let opponent = Marker.getOpponent player
            if board.IsOccupied slot then acc else comparator acc (minimax opponent (markSlot board slot player) (depth+1) (not isMax))
        let scoreOfBoard = boardScore player board
        let score = if isMax then scoreOfBoard - depth else scoreOfBoard + depth
        if score <> 0 || not (areMovesLeft board) then score
        elif isMax then List.fold (minimaxFolder max) (-1000) slots
        else List.fold (minimaxFolder min) 1000 slots

      let folder acc slot =
        if board.IsOccupied slot then acc else
            let moveVal = minimax (Marker.getOpponent player) (markSlot board slot player) 0 false
            if moveVal > fst acc then (moveVal, slot) else acc

      List.fold folder (-1000, 0) slots |> snd