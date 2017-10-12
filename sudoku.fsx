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

type Symbol = Symbol of int

type CellState =
    {
        Symbols : Symbol Set
        IsDirty : bool
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CellState =
    let toString (cs : CellState) =
        match Set.count cs.Symbols with
        | 0 -> die "entry has no possible values"
        | 1 -> cs.Symbols |> Seq.exactlyOne |> fun (Symbol s) -> sprintf "%d" s
        | 9 -> " "
        | c -> "."

    let make s = { Symbols = s; IsDirty = true }

    let clean cs = { cs with IsDirty = false }

    let private unset = [1 .. 9] |> Seq.map Symbol |> Set |> make
        
    let makeExact = Seq.singleton >> Set >> make

    let parse =
        function
        | c when c >= '1' && c <= '9' ->
            int c - int '0' |> Symbol |> makeExact
        | ' ' ->
            unset
        | x ->
            dief "parse error: %A" x

    let isFullyKnown (cs : CellState) =
        Set.count cs.Symbols = 1

    let toOption (cs : CellState) =
        if Set.count cs.Symbols = 1 then cs.Symbols |> Seq.exactlyOne |> Some else None

    let hasExactValue value (cs : CellState) =
        Set.count cs.Symbols = 1 && cs.Symbols |> Seq.exactlyOne = value

    let mightBe value (cs : CellState) = Set.contains value cs.Symbols

    let value (cs : CellState) = cs.Symbols

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

type CycleDefinition = CycleDefinition of Projection * int Set * Symbol Set

module Solver =
    /// A cycle is a group of N cells that must contain N known values between them (ie those values can't be elsewhere).
    /// NB if this is called on a full row the final set logically must contain all values.

    /// Consider the possibilities for each cell in this row as a boolean matrix:
    /// cellnumber  1 2 3 4 5 6 7 8 9
    /// 1 possible  1 0 1 1 1 1 1 1 1
    /// 2 possible  1 1 0 0 0 0 0 0 0
    /// 3 possible  0 1 1 0 0 0 0 0 0
    /// 4 possible  0 0 0 1 1 0 0 0 0
    /// ...
    /// There are two types of cycle:
    /// 1. A group of N columns exists for which their row-wise union only contains N items
    /// 2. A group of N rows exists for which their column-wise union only contains N items
    ///
    /// Our overall search strategy starts by looking for small cycles and only extending to bigger ones once the small ones stop making progress.
    let dummy = ()

    let applyCycle (board : Board) (CycleDefinition (projection, entries, symbols)) : Board option =
        printfn "applyCycle: %A %A %A" projection entries symbols

        let updateSymbol (transform : Symbol Set -> Symbol Set) (board : Board) (index : int) : Board =
            //printfn "updating board:"
            //printfn "%s" (Board.toString board)
            let cs = Projection.read projection board index
            //printfn "cs for index %A = %A" index cs
            let symbols = cs.Symbols |> transform
            //printfn "updated symbols = %A" symbols
            if symbols = cs.Symbols then board else
            let cs' = CellState.make symbols
            let ret = Projection.write projection board index cs'
            //printfn "updated board:"
            //printfn "%s" (Board.toString ret)
            ret

        let restrictToCycleMembers = Set.intersect symbols
        let excludeCycleMembers orig = Set.difference orig symbols
        let applySetTransformation isInCycle = if isInCycle then restrictToCycleMembers else excludeCycleMembers
        let doupdate board index =
            //printfn "entries.Contains %A = %A" index (entries.Contains index)
            let transform = entries.Contains index |> applySetTransformation
            updateSymbol transform board index
        let ret = Seq.fold doupdate board [0..9]
        if ret = board then None else Some ret

    /// Identify a cycle of the given size and return the indices of columns that make it up
    let tryFindNewCycleInProjection (board : Board) (size : int) (projection : Projection) : Board option =
        if size <> 1 then dief "size not implemented %A" size else
        let x index symbol = CycleDefinition (projection, Set.singleton index, Set.singleton symbol) |> applyCycle board
        let group = Projection.readGroup board projection
        let tryEntry index = group.[index] |> CellState.toOption |> Option.bind (x index)
        [0..8] |> Seq.tryPick tryEntry

    let tryFindNewCycle (board : Board) (size : int) : Board option =
        let allProjections = seq { for p in Projection.allModes do for n in [0..9] -> {ProjectionMode = p; ProjectionNumber = n} }
        allProjections |> Seq.tryPick (tryFindNewCycleInProjection board size)

    let step' (board : Board) : Board =
        match tryFindNewCycle board 1 with
        | None -> die "no cycle found - bailing"
        | Some board -> board

//    let rec findNewCycle (size : int) (colsUsed : int list) (valsInCycle : int Set) (colsRemaining : (int Set * int) list) : int list option =
//        match colsRemaining with
//        | [] ->
//            None
//        | (set, colid)::cols ->
//            // Try the first item out as part of the cycle
//            let valsInCycle' = Set.union valsInCycle set
//            if valsInCycle'.Count <= size
//            then
//                let ret = findNewCycle size colid::colsUsed valsInCycle' cols
//                if Option.IsSome ret then ret else



    let possibleValues board row col =
        //printfn "possibleValues %d %d" row col
        let valuesUsedViaProjectionMode mode =
            let p, n = Projection.fromCoords mode row col
            let group = Projection.readGroup board p
            let groupMinusThisCell =
                group
                |> Seq.mapi (fun i x -> i, x)
                |> Seq.choose (fun (i, x) -> if i = n then None else Some x)
            let ret = group |> Seq.choose CellState.toOption |> Set.ofSeq
            //printfn "valuesUsedViaProjectionMode %A = %A" mode ret
            ret

        let allValuesUsed =
            Projection.allModes
            |> List.map valuesUsedViaProjectionMode
            |> Set.unionMany

        let possible = List.init 9 ((+) 1 >> Symbol) |> Set.ofList
        
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
        /// cut down possibilities for each coordinate based on other known cells
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
                Grid.update b check (CellState.make s) row col
                |> OrError.force
                |> Board
                |> Some
            | _ ->
                iterByCoordinate row' col'
        
        /// look for ways to place the given value in the given row
        let rec iterByValue value row =
            let symbol = match value with Symbol s -> s
            if symbol = 10 then None else
            let value', row' = if row = 8 then Symbol(symbol+1), 0 else value, row+1

            let projection = { ProjectionMode = ProjectionMode.Row; ProjectionNumber = row }

            // skip this (value, row) if already known
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
            match iterByValue (Symbol 1) 0 with
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


let checkToken n = Some n
//    let n' = n - 1
//    if n' = 0 then None else Some n'

let rec loop token board  : unit =
    printfn "%s" (Board.toString board)
    match checkToken token with
    | None ->
        printfn "terminating"
    | Some t ->
         (Solver.step' board) |> loop t

Board.parse s2 |> loop 3