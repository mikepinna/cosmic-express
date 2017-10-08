open System

[<AutoOpen>]
module Prelude =

    let cont s = printfn "%s" s ; failwith s

    let dief s = Printf.kprintf cont s

    let die s = dief "%s" s

type 'a OrError = Ok of 'a | Error of string

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OrError =
    let force = function Ok x -> x | Error s -> dief "fatal error: %s" s

type 'a Grid = Grid of 'a array array

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Grid =
    let lookup (Grid grid) row col = grid.[row].[col]

    let update (Grid grid) check value row col =
        let r = grid.[row]
        match check <| r.[col] with
        | Some x -> Error x
        | None ->
            let newRow = Array.copy r
            newRow.[col] <- value
            let g = Array.copy grid
            g.[row] <- newRow
            g |> Grid |> Ok

type CellState = CellState of int Set
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CellState =
    let toString (CellState set) =
        match Set.count set with
        | 0 -> die "entry has no possible values"
        | 1 -> set |> Seq.exactlyOne |> sprintf "%d"
        | 9 -> " "
        | _ -> "."

    let private unset = [1 .. 9] |> Set |> CellState

    let makeExact = Seq.singleton >> Set >> CellState

    let parse =
        function
        | c when c >= '1' && c <= '9' ->
            int c - int '0' |> makeExact
        | ' ' ->
            unset
        | x ->
            dief "parse error: %A" x

    let isFullyKnown (CellState set) =
        Set.count set = 1

    let toOption (CellState set) =
        if Set.count set = 1 then set |> Seq.exactlyOne |> Some else None

    let hasExactValue value (CellState set) =
        Set.count set = 1 && set |> Seq.exactlyOne = value

    let mightBe value (CellState set) = Set.contains value set

    let value (CellState set) = set

type Board = Board of CellState Grid

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Board =
    let groupByThrees =
        function
        | [|a;b;c;d;e;f;g;h;i|] ->
            [|
                [|a;b;c|]
                [|d;e;f|]
                [|g;h;i|]
            |]
        | x -> dief "invalid line: %A" abs

    let parse (lines : string array) : Board =

        
        let parseline = Array.ofSeq >> Array.map CellState.parse
        let parse = Array.map parseline

        let checknine a =
            match Array.length a with 
            | n when n = 9 -> ()
            | n -> dief "wrong number of lines: %d" n

        let parsed = parse lines
        checknine parsed
        parsed |> Array.iter checknine
        
        parsed |> Grid |> Board

    let toString (Board (Grid (grid))) =
        let bigConcat sep a =
            sprintf "%s%s%s" sep (String.concat sep a) sep

        let charGroupToString = Array.map CellState.toString >> bigConcat ""

        let lineToString = groupByThrees >> Array.map charGroupToString >> bigConcat "|"

        let lineGroupToString = Array.map lineToString >> bigConcat Environment.NewLine
        
        let gridToString = groupByThrees >> Array.map lineGroupToString >> bigConcat "+---+---+---+"

        gridToString grid

[<RequireQualifiedAccess>]
type ProjectionMode = Square | Row | Col

type Projection =
    {
        ProjectionMode : ProjectionMode
        ProjectionNumber : int
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Projection =
    let allModes = [ ProjectionMode.Square; ProjectionMode.Row; ProjectionMode.Col ]

    let getOtherModes mode =
        allModes |> List.filter ((<>) mode)

    let to2D i = i % 3, i / 3

    let from2D x y = x + y * 3 

    let toCoords (projection : Projection) i =
        match projection.ProjectionMode with
        | ProjectionMode.Square ->
            let r, c = to2D i
            let rr, cc = to2D projection.ProjectionNumber 
            from2D r rr, from2D c cc
        | ProjectionMode.Row ->
            projection.ProjectionNumber, i
        | ProjectionMode.Col ->
            i, projection.ProjectionNumber

    let fromCoords mode row col =
        match mode with
        | ProjectionMode.Square ->
            let r, rr = to2D row
            let c, cc = to2D col
            let n = from2D rr cc
            let i = from2D r c
            { ProjectionMode = mode; ProjectionNumber = n }, i
        | ProjectionMode.Row ->
            { ProjectionMode = mode; ProjectionNumber = row }, col
        | ProjectionMode.Col ->
            { ProjectionMode = mode; ProjectionNumber = col }, row

    let read projection (Board b) n =
        toCoords projection n ||> Grid.lookup b

    let readGroup board projection =
        Array.init 9 (read projection board)

    let write projection (Board b) n value =
        let check state = if CellState.isFullyKnown state then Some "square already has value" else None
        toCoords projection n ||> Grid.update b check value |> OrError.force |> Board

module Tests =
    let assertEqual x1 x2 =
        if x1 = x2 then () else dief "not equal: %A %A" x1 x2

    let test1 () =
        for n in 0..8 do
            let n' = n |> Projection.to2D ||> Projection.from2D
            assertEqual n n'

    let test2 () =
        for mode in Projection.allModes do
            for row in 0..8 do
            for col in 0..8 do
                let p, n = Projection.fromCoords mode row col
                let row', col' = Projection.toCoords p n

                assertEqual row row'
                assertEqual col col'

    let runall () =
        printfn "running tests"
        test1 ()
        test2 ()

module Solver =
    let possibleValues board row col =
        //printfn "possibleValues %d %d" row col
        let valuesUsedViaProjectionMode mode =
            let p, n = Projection.fromCoords mode row col
            let ret = Projection.readGroup board p |> Seq.choose CellState.toOption |> Set.ofSeq
            //printfn "valuesUsedViaProjectionMode %A = %A" mode ret
            ret

        let allValuesUsed =
            Projection.allModes
            |> List.map valuesUsedViaProjectionMode
            |> Set.unionMany

        let possible = List.init 9 ((+) 1) |> Set.ofList
        
        let ret = Set.difference possible allValuesUsed
        //printfn "ret = %A" ret
        ret

    let possibleLocationsInProjection board projection value =
        let projectionModeContainsValue row col mode =
            Projection.fromCoords mode row col
            |> fst
            |> Projection.readGroup board
            |> Array.exists (CellState.hasExactValue value)

        let otherProjectionModesDontContainValue mode row col =
            Projection.getOtherModes mode
            |> List.map (projectionModeContainsValue row col)
            |> List.reduce (||)
            |> not

        let group = Projection.readGroup board projection |> List.ofArray
        if group |> List.exists (CellState.hasExactValue value) then die "already has location" 

        group
        |> List.mapi (fun index state -> index, state)
        |> List.choose (fun (index, state) -> if CellState.mightBe value state then Some index else None)
        |> List.map (Projection.toCoords projection)
        |> List.filter (fun (r, c) -> otherProjectionModesDontContainValue projection.ProjectionMode r c)

    let step board : Board =
        let rec iterByCoordinate row col =
            if col = 9 then None else
            let row', col' = if row = 8 then 0, col+1 else row+1, col
            let grid = match board with Board b -> b
            let currentState = Grid.lookup grid row col
            if currentState |> CellState.isFullyKnown then iterByCoordinate row' col' else

            let curr = CellState.value currentState

            let pv = possibleValues board row col
            match pv, board with
            | s, _ when s = curr ->
                iterByCoordinate row' col'
            | s, Board b when Set.isProperSubset s curr ->
                let check _ = None
                Grid.update b check (CellState s) row col
                |> OrError.force
                |> Board
                |> Some
            | _ ->
                iterByCoordinate row' col'
        
        let rec iterByValue value row =
            if value = 10 then None else
            let value', row' = if row = 8 then value+1, 0 else value, row+1

            let projection = { ProjectionMode = ProjectionMode.Row; ProjectionNumber = row }

            if Projection.readGroup board projection |> Array.exists (CellState.hasExactValue value) then iterByValue value' row' else

            let possible = possibleLocationsInProjection board projection value
            match possible, board with
            | [], _ ->
                dief "logic error at %A %d" projection row
            | [(row, col)], Board b ->
                let check state = if CellState.mightBe value state then None else Some "logic error"
                Grid.update b check (CellState.makeExact value) row col
                |> OrError.force
                |> Board
                |> Some
            | _ ->
                iterByValue value' row'
        
        match iterByCoordinate 0 0 with
        | Some b -> b
        | None ->
            match iterByValue 1 0 with
            | Some b -> b
            | None ->
                die "no step forward found"
let s =
// https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/solo.html#3x3:4a5c1a8b9_7c5e2b4a7b3a5b9a9b7b3a3b2a8b7a4b3e8c2_3b5a3c4a2
    [| 
        "4 5   1 8"
        "  97   5 "
        "    2  4 "
        "7  3 5  9"
        " 9  7  3 "
        "3  2 8  7"
        " 4  3    "
        " 8   23  "
        "5 3   4 2"
    |]

let s2 =
// https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/solo.html#3x3:a5a2e8a7a1c6_3b7d4a3b2a4b1_2e8_9b9a4b2a7d6b2_2c3a5a7e2a1a
    [| 
        " 5 2     "
        "8 7 1   6"
        "3  7    4"
        " 3  2 4  "
        "12     89"
        "  9 4  2 "
        "7    6  2"
        "2   3 5 7"
        "     2 1 "
    |]

Tests.runall ()
let rec loop board : unit =
    printfn "%s" (Board.toString board)
    Solver.step board |> loop

Board.parse s2 |> loop 