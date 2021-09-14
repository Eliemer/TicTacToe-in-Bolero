module WASMTicTacToeFSharp.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Components

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

let checkWinner (cells : Cell.Cell array) : Mark option =
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


/// The Elmish application's update messages.
type Message =
    | SetSymbol of Cell.Cell * Mark
    | CheckWinner
    | Winner
    | Draw
    | Restart

let update message (model: Model) =
    match message with
    | SetSymbol (cell, mark) -> 
        match model.GameActive with
        | true ->
            match cell.Symbol with
            | Some _ -> model, Cmd.none
            | None ->
                let nextMark = if mark = X then O else X
                Array.set model.Cells cell.Index { cell with Symbol = Some mark }
                { model with CurrentPlayer = nextMark }, Cmd.ofMsg CheckWinner
        | false ->
            model, Cmd.none

    | CheckWinner ->
        match checkWinner model.Cells with
        | Some winner -> 
                {model with Winner = Some winner; GameActive = false}, Cmd.ofMsg Winner
        | None -> 
            let symbols = Array.map (fun (e: Cell.Cell) -> e.Symbol ) model.Cells
            match Array.tryFind Option.isNone symbols with
            | Some _ -> model, Cmd.none // There are still cells to be filled
            | None -> { model with GameActive = false }, Cmd.ofMsg Draw // there are no more empty cells

    | Draw -> {model with Status = $"Game has ended in a Draw!"}, Cmd.none
    | Winner -> {model with Status = $"Player {model.Winner.Value} has won!"}, Cmd.none
    | Restart -> initModel (), Cmd.none


let view (model: Model) (dispatch: Dispatch<Message>) =
    section [] [
        h1 [
            attr.``class`` "game--title"
        ] [
            text "Tic Tac Toe"
            div [
                attr.``class`` "game--container"
            ] [
                // TODO: refactor + cleanup
                forEach model.Cells <| (fun cell -> 
                    Cell.Cell cell (fun _ -> dispatch <| SetSymbol (cell, model.CurrentPlayer)))
            ]
            h2 [
                attr.``class`` "game--status"
            ] [
                text model.Status
            ]
            button [
                attr.``class`` "game--restart"
                on.click (fun _ -> dispatch Restart)
            ] [
                text "Restart Game"
            ]
        ]
    ]

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkProgram (fun _ -> initModel (), Cmd.none) update view
        |> Program.withConsoleTrace
