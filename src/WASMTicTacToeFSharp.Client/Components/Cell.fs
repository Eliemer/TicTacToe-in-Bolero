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

    let Cell (model: Cell) onClickCallback = 
        div [ 
            "data-cell-index" => model.Index
            attr.``class`` "cell"
            on.click onClickCallback
        ] [
            cond model.Symbol <| function
                | Some mark -> text <| $"{mark}"
                | None -> empty
        ]

