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

let trackGraphics =
    let raw = 
        [
            Up, Down,    CellGraphic(' ', '|', ' ', ' ', 'o', ' ', ' ', '|', ' ')
            Up, Left,    CellGraphic(' ', '|', ' ', '-', 'o', ' ', ' ', ' ', ' ')
            Up, Right,   CellGraphic(' ', '|', ' ', ' ', 'o', '-', ' ', ' ', ' ')
            Down, Left,  CellGraphic(' ', ' ', ' ', '-', 'o', ' ', ' ', '|', ' ')
            Down, Right, CellGraphic(' ', ' ', ' ', ' ', 'o', '-', ' ', '|', ' ')
            Left, Right, CellGraphic(' ', ' ', ' ', '-', 'o', '-', ' ', ' ', ' ')
        ]
    raw |> List.collect (fun (a, b, g) -> [(a, b), g; (b, a), g]) |> Map.ofList

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

let mutable debugCounter = 0

type PartialSolution = { Board : Board; Path : (int * int) array }
    with
    static member Make(board : Board) =
        { Board = board; Path = [| board.TrainEntry |]} 
        
    member this.TryAddTrack x y =
        match this.Board with
        | Board board ->
            if not (this.Board.IsValidSquare(x, y)) then None else
            let ok =
                match board.[x].[y] with
                | Empty ->
                    this.Path |> Array.contains (x,y) |> not
                | TrainExit ->
                    true
                | _ ->
                    false
            if ok
            then
                Some <| { this with Path = Array.append this.Path [|x, y|] }
            else
                None

    member this.IsComplete =
        let (x, y) = this.Path |> Array.last
        match this.Board with Board(cells) -> cells.[x].[y] = TrainExit

    member this.HasReachableEnd() =
        let board = match this.Board with Board x -> x
        let trackset = this.Path |> Set.ofArray

        //printfn "check reachability for board"
        //printfn "%s" (this.ToString())
        let pathEnd = this.Path |> Array.last 
        let rec iter (surrounded : Set<int*int>) (todo : Set<int*int>) =
        
            let debug() =
                debugCounter <- debugCounter + 1
                if debugCounter % 10000 = 0
                then
                    printfn "iter %d %d %A %A" (surrounded.Count) (todo.Count) surrounded todo
                    let schar = CellGraphic.make('#')
                    let tchar = CellGraphic.make('+')
                    let o = Map.empty
                    let o = Set.fold (fun m k -> Map.add k schar m) o surrounded
                    let o = Set.fold (fun m k -> Map.add k tchar m) o todo
                    printfn "%s" (this.ToString o)

            if (surrounded.Count + todo.Count > board.Length * board.[0].Length + 4)
            then
                debugCounter <- -1
                debug()
                failwith "oops"
        
            if todo.IsEmpty
            then
                // didn't find end
                debug()
                false
            else
                let first = Set.minElement todo
                //printfn "first = %A" first
                match first with
                | (x, y) when board.[x].[y] = TrainExit ->
                    //printfn "%s" "// found end"
                    debug()
                    true
                | h when surrounded.Contains h ->
                    //printfn "%s" "// item item already processed so move on"
                    iter surrounded (Set.remove h todo)
                | h when h <> pathEnd && trackset.Contains h ->
                    //printfn "%s" "// item clashes with track so move on"
                    iter surrounded (Set.remove h todo)
                | (x, y) when board.[x].[y] = Empty ->
                    //printfn "%s" "// expand neighbours and add this one to surrounded list"
                    let todo' =
                        directionVectors
                        |> List.map (fun (dx,dy) -> x+dx, y+dy)
                        |> List.filter this.Board.IsValidSquare
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

    member this.Children() =
        if this.IsComplete then failwith "can't get children from complete board"
        let x, y = this.Path |> Array.last
        directionVectors
        |> List.choose (fun (dx, dy) -> this.TryAddTrack (x + dx) (y + dy))
        |> List.filter (fun ps -> ps.HasReachableEnd())

    member this.ToGraphic (overrides : Map<int*int,CellGraphic>) =
        let getDirection (x, y) (x', y') =
            match (x'-x, y'-y) with
            | v when v = Left.V  -> Left
            | v when v = Right.V -> Right
            | v when v = Up.V    -> Up
            | v when v = Down.V  -> Down
            | _ -> failwithf "getDirection: illegal move from (%d,%d) to (%d,%d)" x y x' y'
        
        //printfn "this.Path = %A" this.Path
        // make a list of cells the track goes through with the direction from which the train enters
        let trackEntries = this.Path |> Array.pairwise |> Array.map (fun (curr, next) -> next, getDirection next curr)
        //printfn "trackEntries = %A" trackEntries
        // make a list of cells the track goes through with the direction from which it enters AND leaves
        let trackEntriesAndExits = trackEntries |> Array.pairwise |> Array.map (fun ((c,entry), (_,exit)) -> c, entry, exit.Invert)
        //printfn "trackEntriesAndExits = %A" trackEntriesAndExits
        // convert to graphics and put in a map for querying
        let trackAsGraphics = trackEntriesAndExits |> Array.map (fun (c, entry, exit) -> c, trackGraphics.[entry, exit]) |> Map.ofArray
        //printfn "trackAsGraphics = %A" trackAsGraphics

        let transparentBoardGraphic = this.Board.ToTransparentGraphic()

        let makeNonTransparent x y (g: CellGraphic option) =
            match g, trackAsGraphics.TryFind(x, y) with
            | None, None -> match overrides.TryFind(x,y) with Some q -> q | None -> CellGraphic.make('.')
            | Some q, None -> q
            | None, Some q -> q
            | Some _, Some _ -> failwithf "track overlaps with board at (%d,%d)" x y

        this.Board.ToTransparentGraphic() |> Array.mapi(fun x row -> row |> Array.mapi(fun y cell -> cell |> makeNonTransparent x y))
        
    member this.ToGraphic () =
        this.ToGraphic Map.empty

    member this.ToString (overrides : Map<_,_>) =
        let doRow (r : CellGraphic array) =
            let row1 = r |> Array.fold (fun s (CellGraphic(a, b, c, _, _, _, _, _, _)) -> sprintf "%s%c%c%c" s a b c) ""
            let row2 = r |> Array.fold (fun s (CellGraphic(_, _, _, d, e, f, _, _, _)) -> sprintf "%s%c%c%c" s d e f) ""
            let row3 = r |> Array.fold (fun s (CellGraphic(_, _, _, _, _, _, g, h, i)) -> sprintf "%s%c%c%c" s g h i) ""
            [|row1; row2; row3|]
        let rows = this.ToGraphic overrides |> Array.collect doRow
        Array.fold (fun a b -> sprintf "%s%s%s" a Environment.NewLine b) (sprintf "track: %A" this.Path) rows

    override this.ToString() =
        this.ToString Map.empty

let countErrors (s : PartialSolution) : int =
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

    let rec iter step (trainState : AlienType option) (aliens : Map<int*int,AlienType>) (boxes : Map<int*int,AlienType>) =
        if step = s.Path.Length
        then
            let bools =
                [
                    trainState <> None
                    not s.IsComplete
                ]
                |> List.map (function true -> 1 | false -> 0)
                |> List.sum
            let ints = 
                [
                    aliens.Count
                    boxes.Count
                ]
                |> List.sum
            bools + ints
            
        else
            let (x, y) = s.Path.[step]
            let trainState, boxes  = dropAlien x y trainState boxes
            let trainState, aliens = getAlien  x y trainState aliens
            iter (step + 1) trainState aliens boxes

    let aliens = s.Board.Aliens |> Map.ofArray
    let boxes  = s.Board.Boxes |> Map.ofArray

    iter 0 None aliens boxes

let testCompleteSolution (s : PartialSolution) : bool =
    countErrors s = 0

let doTestCompleteSolution (s : PartialSolution) : bool =
    printfn "doTestCompleteSolution"
    printfn "%s" <| s.ToString()
    let ret = testCompleteSolution s
    printfn "%A" ret
    ret

let rec backtrackingSolver (ps : PartialSolution) =
    if ps.IsComplete
    then
        if testCompleteSolution ps
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
        getFirst <| ps.Children()

let shortestPathSolver (ps : PartialSolution) =
    let rec iter (current : PartialSolution list) next =
        match current with
        | [] ->
            iter next []
        | c::_ when c.IsComplete && testCompleteSolution c ->
            Some c
        | c::cs when c.IsComplete ->
            iter cs next
        | c::cs ->
            iter cs (c.Children() @ next)

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
        if minps.IsComplete
        then
            if testCompleteSolution minps
            then
                Some minps
            else
                iter set'
        else
            minps.Children()
            |> List.map makeComparable
            |> List.fold (fun s c -> Set.add c s) set'
            |> iter
        
    iter (Set.add (makeComparable ps) Set.empty)
        
let lossFunction (ps : PartialSolution) =
    -((countErrors ps * 3) + ps.Path.Length)

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



let partialSolution1 = andromeda3 |> Board.Parse |> PartialSolution.Make

partialSolution1.ToString() |> printfn "%s"

let soln = shortestPathSolver2 partialSolution1  
match soln with Some s -> printfn "found solution!"; printfn "%s" (s.ToString()) | None -> printfn "no solution found"






