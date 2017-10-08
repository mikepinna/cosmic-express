open System

type Player = X | O

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Player =
    let other = function X -> O | O -> X

type 'a OrError = Ok of 'a | Error of string

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OrError =
    let force = function Ok x -> x | Error s -> failwithf "fatal error: %s" s

// lookup order is always horizontal then vertical, ie row then column

type Move =
    {
        Player : Player
        IsVertical : bool
        Row : int
        Col : int
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Move =
    let rec getInput description parser =
        printfn "%s" description
        let line = Console.ReadLine()
        match parser line with Some x -> x | None -> getInput description parser

    let rec read player : Move =
        printfn "Move for player %A" player
        printfn "Enter row number"
        match Int32.TryParse <| Console.ReadLine () with
        | false, _ -> read player
        | true, row ->
        printfn "Enter column number"
        match Int32.TryParse <| Console.ReadLine () with
        | false, _ -> read player
        | true, col ->
        printfn "Enter v or h for direction"
        match Console.ReadLine () with
        | "v" -> {Player = player; IsVertical = true;  Row = row; Col = col}
        | "h" -> {Player = player; IsVertical = false; Row = row; Col = col}
        | _ -> read player

type 'a Grid = Grid of 'a array array

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Grid =
    let lookup (Grid grid) row col = grid.[row].[col]

    let update (Grid grid) row col check value =
        let r = grid.[row]
        match check <| r.[col] with
        | Some x -> Error x
        | None ->
            let newRow = Array.copy r
            newRow.[col] <- value
            let g = Array.copy grid
            g.[row] <- newRow
            g |> Grid |> Ok

type Board =
    {
        Size : int
        H : bool Grid
        V : bool Grid
        Complete : Player option Grid
        Scores : Map<Player, int>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Board =
    let make (size : int) =
        let h = Array.init size (fun _ -> Array.init (size-1) (fun _ -> false))
        let v = Array.init (size-1) (fun _ -> Array.init size (fun _ -> false))
        let c = Array.init (size-1) (fun _ -> Array.init (size-1) (fun _ -> None))
        let scores = [|O,0;X,0|] |> Map.ofArray
        {Size = size; H = Grid h; V = Grid v; Complete = Grid c; Scores = scores}

    let toString (board : Board) =
        let hRowToString row =
            Seq.init (board.Size-1) (fun col -> if Grid.lookup board.H row col then "-" else " ")
            |> String.concat "."
            |> fun text -> sprintf "%d .%s. %d" row text row
        let po2str =
            function
            | Some X -> "X"
            | Some O -> "O"
            | None   -> " "
        let vRowToString row =
            let colPairs =
                Seq.init (board.Size - 1) id
                |> Seq.collect (fun col -> [(if Grid.lookup board.V row col then "|" else " "); Grid.lookup board.Complete row col |> po2str])
            seq {
                yield! colPairs
                yield if Grid.lookup board.V row (board.Size-1) then "|" else " "
            }
            |> String.concat ""
            |> sprintf "  %s"
        let rowPairs =
            Seq.init (board.Size - 1) id
            |> Seq.collect (fun row -> [hRowToString row; vRowToString row])
        let titleRow =
            Seq.init (board.Size) (sprintf "%d")
            |> String.concat " "
            |> sprintf "  %s"
        let scores =
            board.Scores
            |> Seq.map (function KeyValue(k, v) -> sprintf "%A: %d" k v)
            |> String.concat "; "
        seq {
            yield titleRow
            yield! rowPairs
            yield hRowToString (board.Size-1)
            yield titleRow
            yield sprintf "Scores: %s" scores
        }
        |> String.concat Environment.NewLine

    let checkSquare (row : int) (col : int) (player : Player) (board : Board) : Board OrError =
        printfn "check square %d %d" row col
        if
            row = -1 ||
            col = -1 ||
            row = (board.Size - 1) ||
            col = (board.Size - 1)
        then
            Ok board
        else if
            Grid.lookup board.V row col &&
            Grid.lookup board.V row (col+1) &&
            Grid.lookup board.H row col &&
            Grid.lookup board.H (row+1) col
        then
            let check = Option.map (sprintf "Square already owned (by %A)")
            match Grid.update board.Complete row col check (Some player) with
            | Error x -> Error x
            | Ok x ->
                let score = board.Scores.[player] + 1
                let scores = Map.add player score board.Scores
                Ok {board with Complete = x; Scores = scores}
        else
            Ok board

    let applyMove (move : Move) (board : Board) : Board OrError =
        let check = function true -> Some "Line already present" | _ -> None
        if move.IsVertical
        then
            let v = Grid.update board.V move.Row move.Col check true
            match v with
            | Error x -> Error x
            | Ok x ->
                let board = {board with V = x}
                let board = board |> checkSquare move.Row (move.Col - 1) move.Player |> OrError.force
                let board = board |> checkSquare move.Row  move.Col      move.Player |> OrError.force 
                Ok board
        else
            match Grid.update board.H move.Row move.Col check true with
            | Error x -> Error x
            | Ok x ->
                let board = {board with H = x}
                let board = board |> checkSquare (move.Row - 1) move.Col move.Player |> OrError.force
                let board = board |> checkSquare  move.Row      move.Col move.Player |> OrError.force 
                Ok board

let run : unit =
    let rec loop board player =
        Board.toString board |> printfn "%s"
        let move = Move.read player
        match Board.applyMove move board with
        | Ok b -> loop b (Player.other player)
        | Error s -> printfn "%s" s ; loop board player

    let b = Board.make 10
    loop b X
