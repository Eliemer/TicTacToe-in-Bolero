namespace Components

open Bolero
open Bolero.Html

type Mark = X | O

module Cell =
    type Cell =
        {
            Symbol : Mark option
            Index : int
        }

    let initCell (model: Cell) onClickCallback =
        div [
            "data-cell-index" => model.Index
            attr.``class`` "cell"
            on.click onClickCallback
        ] [
            cond model.Symbol <| function
                | Some mark -> text <| $"{mark}"
                | None -> empty
        ]

    let checkWinner (cells : Cell array) : Mark option =
        let winCond = [
            // horizontal
            [0;1;2]
            [3;4;5]
            [6;7;8]
            // vertical
            [0;3;6]
            [1;4;7]
            [2;5;8]
            // diagonal
            [0;4;8]
            [2;4;6]
        ]

        seq { for i in [0..7] do
                let winC = winCond.[i]
                let a = cells.[winC.[0]].Symbol
                let b = cells.[winC.[1]].Symbol
                let c = cells.[winC.[2]].Symbol

                if a.IsSome && a = b && b = c then a else None }
        |> Seq.tryFind Option.isSome
        |> Option.flatten