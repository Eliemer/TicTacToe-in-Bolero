module WASMTicTacToeFSharp.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Components

type Message =
    | SetSymbol of Cell.Cell * Mark
    | CheckWinner
    | Winner
    | Draw
    | Restart

let update (message: Message) (model: Model.Model) =
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
        match Cell.checkWinner model.Cells with
        | Some winner ->
                {model with Winner = Some winner; GameActive = false}, Cmd.ofMsg Winner
        | None ->
            let symbols = Array.map (fun (e: Cell.Cell) -> e.Symbol ) model.Cells
            match Array.tryFind Option.isNone symbols with
            | Some _ -> model, Cmd.none
            // there are no more empty cells, call a Draw
            | None -> { model with GameActive = false }, Cmd.ofMsg Draw

    | Draw -> {model with Status = $"Game has ended in a Draw!"}, Cmd.none
    | Winner -> {model with Status = $"Player {model.Winner.Value} has won!"}, Cmd.none
    | Restart -> Model.initModel (), Cmd.none

let view (model: Model.Model) (dispatch: Dispatch<Message>) =
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
                    Cell.initCell cell (fun _ -> dispatch <| SetSymbol (cell, model.CurrentPlayer)))
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
        footer [] [
            text $"Tic Tac Toe favicon by Wolf B{'\u00f6'}se from the Noun Project"
        ]
    ]

type MyApp() =
    inherit ProgramComponent<Model.Model, Message>()

    override this.Program =
        Program.mkProgram (fun _ -> Model.initModel (), Cmd.none) update view
        |> Program.withConsoleTrace