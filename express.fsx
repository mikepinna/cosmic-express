open System
open System.Collections.Generic

type AlienType = AlienType of char

type CellGraphic = CellGraphic of char * char * char * char * char * char * char * char * char with
    static member make c = CellGraphic(c, c, c, c, c, c, c, c, c)

type FixedCell =
    |    Alien of AlienType
    |    Box of AlienType
    |    TrainEntry
    |    TrainExit
    |    Empty
    with
    static member private mapping =
        [
            'a', Alien(AlienType('a'))
            'A', Box(AlienType('a'))
            'b', Alien(AlienType('b'))
            'B', Box(AlienType('b'))
            'n', TrainEntry
            'x', TrainExit
            ' ', Empty
        ]
    static member private str2val =
        FixedCell.mapping |> Map.ofList
    static member private val2str =
        FixedCell.mapping |> List.map (fun (a,b) -> (b,a)) |> Map.ofList
    static member Parse x =
        FixedCell.str2val.Item x
    member this.ToChar =
        FixedCell.val2str.Item this
    member this.ToTransparentGraphic =
        match this with
        | Empty ->
            None
        | _ -> this.ToChar |> CellGraphic.make |> Some
    static member EmptyGraphic =
        FixedCell.Empty.ToChar |> CellGraphic.make

type PartialCell =
    | Fixed of FixedCell
    | Track

        
type Direction = Up | Down | Left | Right with
    member this.Invert =
        match this with Up -> Down | Down -> Up | Left -> Right | Right -> Left
    member this.V =
        match this with Up -> -1,0 | Down -> 1,0 | Left -> 0,-1 | Right -> 0,1

let directionVectors = [ Left.V ; Right.V ; Up.V ; Down.V ]

let trackGraphics (n : int) =
    let raw c = 
        [
            Up, Down,    CellGraphic(' ', '|', ' ', ' ', c, ' ', ' ', '|', ' ')
            Up, Left,    CellGraphic(' ', '|', ' ', '-', c, ' ', ' ', ' ', ' ')
            Up, Right,   CellGraphic(' ', '|', ' ', ' ', c, '-', ' ', ' ', ' ')
            Down, Left,  CellGraphic(' ', ' ', ' ', '-', c, ' ', ' ', '|', ' ')
            Down, Right, CellGraphic(' ', ' ', ' ', ' ', c, '-', ' ', '|', ' ')
            Left, Right, CellGraphic(' ', ' ', ' ', '-', c, '-', ' ', ' ', ' ')
        ]
    
    let c =
        //match trainState with None -> '.' | Some(AlienType(alien)) -> alien
        n.ToString() |> Seq.last
    raw c |> List.collect (fun (a, b, g) -> [(a, b), g; (b, a), g]) |> Map.ofList

type Board = Board of FixedCell array array
    with
    static member Parse (board : string array) : Board =
        let parseRow (row : string) =
            row.ToCharArray() |> Array.map FixedCell.Parse
        board |> Array.map parseRow |> Board
        
    member this.IsValidSquare (x, y) =
        let board = match this with Board b -> b
        x >= 0 && y >= 0 && x < board.Length && y < board.[0].Length

    member this.Find p =
        let arrayfind predicate b =
            let findInRow i = Array.mapi(fun j cell -> predicate cell |> Option.map (fun x -> ((i, j), x))) >> Array.choose id
            b |> Array.mapi findInRow |> Array.concat
        match this with Board b -> b |> arrayfind p
    
    member this.TrainEntry =
        this.Find (function TrainEntry -> Some () | _ -> None) |> Seq.exactlyOne |> fst

    member this.TrainExits =
        this.Find (function TrainExit -> Some () | _ -> None)  |> Seq.map fst

    member this.Aliens =
        this.Find (function Alien(x) -> Some x | _ -> None)

    member this.Boxes =
        this.Find (function Box(x) -> Some x | _ -> None)

    member this.ToTransparentGraphic() : CellGraphic option array array =
        match this with Board b -> b |> Array.map (fun row -> row |> Array.map (fun cell -> cell.ToTransparentGraphic))



type Path = (int * int) array

type PartialSolution =
    {
        Board : Board
        Path : ((int * int) * AlienType option * int) array
        RemainingAliens : Map<int*int,AlienType>
        RemainingBoxes : Map<int*int,AlienType>
    }

module Sim =
    let dropAlien x y (trainState : AlienType option) (boxes : Map<int*int,AlienType>) =
        match trainState with
        | None ->
            trainState, boxes
        | Some ts ->
            let rec dropIter dirs =
                match dirs with
                | [] ->
                    trainState, boxes
                | (dx,dy)::dirs' ->
                    let x' = x + dx
                    let y' = y + dy
                    if boxes.TryFind(x', y') = Some ts
                    then
                        None, boxes.Remove(x', y')
                    else
                        dropIter dirs'

            dropIter directionVectors

    let getAlien x y (trainState : AlienType option) (aliens : Map<int*int,AlienType>) =
        match trainState with
        | Some _ ->
            trainState, aliens
        | None ->
            let rec getIter dirs =
                match dirs with
                | [] ->
                    trainState, aliens
                | (dx,dy)::dirs' ->
                    let x' = x + dx
                    let y' = y + dy
                    match aliens.TryFind(x', y') with
                    | None ->
                        getIter dirs'
                    | Some a ->
                        Some a, aliens.Remove(x', y')

            getIter directionVectors

    let runTrain (x,y) (trainState : AlienType option) (aliens : Map<int*int,AlienType>) (boxes : Map<int*int,AlienType>) =
        let trainState, boxes  = dropAlien x y trainState boxes
        let trainState, aliens = getAlien  x y trainState aliens
        trainState, aliens, boxes

let mutable totalChildrenMade = 0

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PartialSolution =



    let getBoard (ps : PartialSolution) = ps.Board
    let getPath (ps : PartialSolution) : (int * int) array =
        ps.Path |> Array.map (fun (a, _, _) -> a)
    let getTrainState (ps : PartialSolution) : AlienType option =
        ps.Path |> Array.last |> (fun (_, b, _) -> b)
    let getPathWithTrainState (ps : PartialSolution) : ((int * int) * AlienType option) array =
        ps.Path |> Array.map (fun (a, b, _) -> (a, b))
    let getTransitionsRemaining (ps : PartialSolution) : int array =
        ps.Path |> Array.map (fun (_, _, c) -> c)
    let getRemainingAliens (ps : PartialSolution) = ps.RemainingAliens
    let getRemainingBoxes (ps : PartialSolution) = ps.RemainingBoxes
    
    let makeChild coords (ps : PartialSolution) =
        let trainState, remainingAliens, remainingBoxes = Sim.runTrain coords (ps |> getTrainState) ps.RemainingAliens ps.RemainingBoxes
        {
            Board = ps.Board
            Path = Array.append ps.Path [|coords, trainState, remainingAliens.Count + remainingBoxes.Count|]
            RemainingAliens = remainingAliens
            RemainingBoxes = remainingBoxes
        }

    let make (board : Board) =
        let aliens = board.Aliens |> Map.ofArray
        let boxes  = board.Boxes |> Map.ofArray
        let trainState, remainingAliens, remainingBoxes = Sim.runTrain board.TrainEntry None aliens boxes
        let transitionsRemaining = remainingAliens.Count + remainingBoxes.Count
        { Board = board; Path = [| board.TrainEntry, trainState, transitionsRemaining |]; RemainingAliens = remainingAliens; RemainingBoxes = remainingBoxes }
        
    let toGraphicWithOverrides (overrides : Map<int*int,CellGraphic>) ps =
        let getDirection (x, y) (x', y') =
            match (x'-x, y'-y) with
            | v when v = Left.V  -> Left
            | v when v = Right.V -> Right
            | v when v = Up.V    -> Up
            | v when v = Down.V  -> Down
            | _ -> failwithf "getDirection: illegal move from (%d,%d) to (%d,%d)" x y x' y'
        
        //printfn "this.Path = %A" this.Path
        // make a list of cells the track goes through with the direction from which the train enters
        let trackEntries =
            getPath ps
            |> Array.pairwise
            |> Array.map (fun ((curr), (next)) -> next, getDirection next curr)
        //printfn "trackEntries = %A" trackEntries
        // make a list of cells the track goes through with the direction from which it enters AND leaves
        let trackEntriesAndExits =
            trackEntries
            |> Array.pairwise
            |> Array.map (fun ((c,entry), (_,exit)) -> c, entry, exit.Invert)
        //printfn "trackEntriesAndExits = %A" trackEntriesAndExits
        // convert to graphics and put in a map for querying
        let transitionRemainings = getTransitionsRemaining ps |> Array.skip 1 |> Array.take (trackEntriesAndExits.Length)

        let trackAsGraphics = 
            trackEntriesAndExits
            |> Array.zip transitionRemainings
            |> Array.map (fun (n, (c, entry, exit)) -> c, (trackGraphics n).[entry, exit])
            |> Map.ofArray
        //printfn "trackAsGraphics = %A" trackAsGraphics

        let transparentBoardGraphic = (getBoard ps).ToTransparentGraphic()

        let makeNonTransparent (x, y) (g: CellGraphic option) =
            match g, trackAsGraphics.TryFind(x, y) with
            | None, None -> match overrides.TryFind(x,y) with Some q -> q | None -> CellGraphic.make('.')
            | Some q, None -> q
            | None, Some q -> q
            | Some _, Some _ -> failwithf "track overlaps with board at (%d,%d)" x y

        (getBoard ps).ToTransparentGraphic() |> Array.mapi(fun x row -> row |> Array.mapi(fun y cell -> cell |> makeNonTransparent (x, y)))
        
    let toGraphic = toGraphicWithOverrides Map.empty
        
    let toStringWithOverrides overrides ps =
        let doRow (r : CellGraphic array) =
            let row1 = r |> Array.fold (fun s (CellGraphic(a, b, c, _, _, _, _, _, _)) -> sprintf "%s%c%c%c" s a b c) ""
            let row2 = r |> Array.fold (fun s (CellGraphic(_, _, _, d, e, f, _, _, _)) -> sprintf "%s%c%c%c" s d e f) ""
            let row3 = r |> Array.fold (fun s (CellGraphic(_, _, _, _, _, _, g, h, i)) -> sprintf "%s%c%c%c" s g h i) ""
            [|row1; row2; row3|]
        let rows = ps |> toGraphicWithOverrides overrides |> Array.collect doRow
        Array.fold (fun a b -> sprintf "%s%s%s" a Environment.NewLine b) (sprintf "track: %A" (getPath ps)) rows

    let toString = toStringWithOverrides Map.empty
  
    let mutable debugCounter = 0
    let debug ps =
        debugCounter <- debugCounter + 1
        if debugCounter % 10000 = 0
        then
            printfn "%s" (ps |> toString)
    
    let tryAddTrack x y ps =
        match getBoard ps with
        | Board board ->
            if not (ps.Board.IsValidSquare(x, y)) then None else
            let ok =
                match board.[x].[y] with
                | Empty ->
                    getPath ps |> Array.contains (x,y) |> not
                | TrainExit ->
                    true
                | _ ->
                    false
            if ok
            then
                totalChildrenMade <- totalChildrenMade + 1
                Some <| makeChild (x,y) ps
            else
                None

    let isComplete ps =
        let (x, y) = ps |> getPath |> Array.last
        match getBoard ps with Board(cells) -> cells.[x].[y] = TrainExit

    let latestMoveWastesTrack ps = 
        //printfn ""
        //printfn "latestMoveWastesTrack"
        let transitionsRemaining = ps |> getTransitionsRemaining
        //printfn "transitionsRemaining = %A" transitionsRemaining
        let transitionsRemainingAtEnd = transitionsRemaining |> Array.last
        //printfn "transitionsRemainingAtEnd = %A" transitionsRemainingAtEnd
        let x, y = ps |> getPath |> Array.last
        //printfn "x, y = %A, %A" x y
        let transitionsRemainingMap =
            ps.Path
            |> Array.take (transitionsRemaining.Length - 2)
            |> Array.map (fun (a, _, c) -> a, c)
            |> Map.ofArray
        //printfn "transitionsRemainingMap = %A" transitionsRemainingMap
        let neighbours = 
            directionVectors
            |> List.map (fun (dx, dy) -> x+dx, y+dy)
        //printfn "neighbours = %A" neighbours
        let transitionsRemainingAtNeighbouringTrack =
            neighbours
            |> List.choose transitionsRemainingMap.TryFind
        //printfn "transitionsRemainingAtNeighbouringTrack = %A" transitionsRemainingAtNeighbouringTrack
        let ret = transitionsRemainingAtNeighbouringTrack |> List.exists ((=) transitionsRemainingAtEnd)
        //printfn "ret = %A" ret

        //if ret then printfn "%A" (toString ps); failwith ""

        ret

    let hasReachableEnd ps =
        let trackset = ps |> getPath |> Set.ofArray

        //printfn "check reachability for board"
        //printfn "%s" (this.ToString())
        let pathEnd = ps |> getPath |> Array.last 
        let rec iter (surrounded : Set<int*int>) (todo : Set<int*int>) =
        
            if todo.IsEmpty
            then
                // didn't find end
                false
            else
                let first = Set.minElement todo
                let b = match getBoard ps with Board b -> b
                //printfn "first = %A" first
                match first with
                | (x, y) when b.[x].[y] = TrainExit ->
                    //printfn "%s" "// found end"
                    true
                | h when surrounded.Contains h ->
                    //printfn "%s" "// item item already processed so move on"
                    iter surrounded (Set.remove h todo)
                | h when h <> pathEnd && trackset.Contains h ->
                    //printfn "%s" "// item clashes with track so move on"
                    iter surrounded (Set.remove h todo)
                | (x, y) when b.[x].[y] = Empty ->
                    //printfn "%s" "// expand neighbours and add this one to surrounded list"
                    let todo' =
                        directionVectors
                        |> List.map (fun (dx,dy) -> x+dx, y+dy)
                        |> List.filter (getBoard ps).IsValidSquare
                        |> List.filter (surrounded.Contains >> not)
                        |> Set.ofList
                        |> Set.union todo
                        |> Set.remove (x,y)
                    
                    let surrounded' = Set.add (x, y) surrounded
                    iter surrounded' todo'
                | h ->
                    //printfn "%s" "// this item tramples on a bit of board, skip it"
                    iter surrounded (Set.remove h todo)
                       
        let ret = iter Set.empty (Set.add pathEnd Set.empty)

        //printfn "%A" ret
        //if not ret then failwith "hi"
        ret

    let children ps =
        debug ps
        if isComplete ps then failwith "can't get children from complete board"
        let x, y = ps |> getPath |> Array.last
        directionVectors
        |> List.choose (fun (dx, dy) -> ps |> tryAddTrack (x + dx) (y + dy))
   //     |> List.filter hasReachableEnd
        |> List.filter (latestMoveWastesTrack >> not)

    let countErrors (ps : PartialSolution) : int =
        let bools =
            [
                (getTrainState ps) <> None
                isComplete ps |> not
            ]
            |> List.map (function true -> 1 | false -> 0)
            |> List.sum
        let ints = 
            [
                (getRemainingAliens ps).Count
                (getRemainingBoxes ps).Count
            ]
            |> List.sum
        bools + ints

    let testCompleteSolution (s : PartialSolution) : bool =
        countErrors s = 0

    let doTestCompleteSolution (s : PartialSolution) : bool =
        printfn "doTestCompleteSolution"
        printfn "%s" <| toString s
        let ret = testCompleteSolution s
        printfn "%A" ret
        ret

let rec backtrackingSolver (ps : PartialSolution) =
    if PartialSolution.isComplete ps
    then
        if PartialSolution.testCompleteSolution ps
        then Some ps
        else None
    else
        let rec getFirst l = 
            match l with
            | [] ->
                None
            | h::l' ->
                match backtrackingSolver h with
                | Some s -> Some s
                | None -> getFirst l'
        ps |> PartialSolution.children |> getFirst

let shortestPathSolver (ps : PartialSolution) =
    let rec iter (current : PartialSolution list) next =
        match current with
        | [] ->
            iter next []
        | c::_ when PartialSolution.isComplete c && PartialSolution.testCompleteSolution c ->
            Some c
        | c::cs when PartialSolution.isComplete c ->
            iter cs next
        | c::cs ->
            iter cs (PartialSolution.children c @ next)

    iter [ps] []

type 'a Comparable = Comparable of 'a * ('a -> 'a -> int) with
    interface IComparable with
        member this.CompareTo(other : obj) =
            match this, (other:?> 'a Comparable) with
            | Comparable(t, cmp), Comparable(o, _) -> cmp t o
    member this.Value = match this with Comparable (x,_) -> x

let aStarSolver (g : PartialSolution -> int) (h : PartialSolution -> int) (ps : PartialSolution) =
    let gh ps = g ps + h ps
    let cmp x y =
        if x.Path = y.Path then 0 else
        let d = gh y - gh x
        if d <> 0 then d else
        (x.Path.GetHashCode() - y.Path.GetHashCode())
    let makeComparable ps = Comparable (ps, cmp)
    
    let rec iter (set : PartialSolution Comparable Set) =
        if Set.isEmpty set then None else
        let min = Set.minElement set
        let minps = min.Value
        let max = Set.maxElement set
        let maxps = max.Value
        let set' = set.Remove min

        //printfn "set.Count = %A" set.Count
        //printfn "%s" (minps.ToString())
        //printfn "minps.Path.Length = %A" minps.Path.Length
        //printfn "maxps.Path.Length = %A" maxps.Path.Length
        if PartialSolution.isComplete minps
        then
            if PartialSolution.testCompleteSolution minps
            then
                Some minps
            else
                iter set'
        else
            PartialSolution.children minps
            |> List.map makeComparable
            |> List.fold (fun s c -> Set.add c s) set'
            |> iter
        
    iter (Set.add (makeComparable ps) Set.empty)
        
let lossFunction (ps : PartialSolution) =
    -((PartialSolution.countErrors ps * 10) + ps.Path.Length)

let shortestPathSolver2 = aStarSolver (lossFunction) (fun ps -> 0)

//partialSolution1 |> expand 11 |> Seq.iter (fun p -> if p.IsComplete then doTestCompleteSolution p |> ignore)



let b1 =
    [|
        "      "
        "      "
        " a   x"
        "n     "
        "    A "
        "      "
        "      "
    |]

let andromeda3 =
    [|
        "         "
        " b  A  b "
        "         "
        "n       x"
        "         "
        " B  a  B "
        "         "
    |]

let andromeda10 =
    [|
        "         "
        "        n"
        "  aaaaa  "
        "         "
        "         "
        "         "
        "  AAAAA  "
        "        x"
        "         "
    |]

let andromeda11 =
    [|
        "          "
        "          "
        "   aAAa   "
        "n        x"
        "   aAAa   "
        "          "
        "          "
    |]



let partialSolution1 = andromeda11 |> Board.Parse |> PartialSolution.make

partialSolution1 |> PartialSolution.toString |> printfn "%s"

let soln = shortestPathSolver2 partialSolution1  
match soln with Some s -> printfn "found solution!"; printfn "%s" (s |> PartialSolution.toString) | None -> printfn "no solution found"


printfn "total children made: %A" totalChildrenMade



