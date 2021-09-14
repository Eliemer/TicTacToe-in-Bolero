namespace Components

module Model =
    type Model =
        {
            Cells: Cell.Cell array
            CurrentPlayer: Mark
            Winner: Mark option
            Status: string
            GameActive: bool
        }

    let initModel () =
        {
            Cells = Array.init 9 (fun idx -> { Symbol = None; Index = idx })
            CurrentPlayer = X
            Winner = None
            Status = ""
            GameActive = true
        }