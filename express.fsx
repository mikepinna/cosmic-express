open System
open System.Collections.Generic

let die x = printfn "die: %s" x ; failwith x

type ITextBasedAdventureState<'s> =
    abstract member Parent : 's option
    abstract member Children : 's []
    abstract member IsSolution : bool

module TextBasedAdventure =
    let printChild (n : int) (s : ITextBasedAdventureState<'s>) =
        printfn "child %d" n
        printfn "%s" <| s.ToString()
        printfn ""

    let rec iter (s : ITextBasedAdventureState<'s>) =
        if s.IsSolution then printfn "Congratulations!  You have found a solution." else

        printfn ""
        printfn "Press 'p' to go back to parent or choose one of the following %d children:" s.Children.Length
        Array.iteri printChild s.Children
        let line = Console.ReadLine()
        match line, Int32.TryParse line with
        | "p", _ ->
            match s.Parent with
            | None ->
                printfn "no parent!"
                iter s
            | Some p ->
                printfn "going to parent"
                p |> iter
        | _, (true, n) when n < s.Children.Length ->
            printfn "going to child %d" n
            s.Children.[n] |> iter
        | _ ->
            printfn "unparseable input: %A" line
            iter s

    let run (s : ITextBasedAdventureState<'s>) =
        printfn "initial state:"
        printfn "%A" <| s.ToString()
        iter s

type AlienType = AlienType of char

type Coordinate =
    { 
        X : int
        Y : int
    }

type Direction =
    {
        DX : int
        DY : int
    }

type TrackElement =
    | TrackSingle     of Coordinate
    | TrackTerminator of Coordinate * Direction
    | TrackMiddle     of Coordinate * Direction * Direction
    
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Direction =
    let private make dx dy =
        match dx, dy with
        | -1, 0 | +1, 0 | 0, -1 | 0, +1 -> {DX=dx; DY=dy}
        | _ -> die <| sprintf "invalid delta %d %d" dx dy

    let Up =
        {DX = -1; DY = 0}
    let Down =
        {DX = +1; DY = 0}
    let Left =
        {DX = 0; DY = -1}
    let Right =
        {DX = 0; DY = +1}

    let invert (d : Direction) =
        {DX = -d.DX; DY = -d.DY}

    let fromCoordinateDelta (c1 : Coordinate) (c2 : Coordinate) =
        make (c2.X - c1.X) (c2.Y - c1.Y)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Coordinate =
    let addDirection (d : Direction) (c : Coordinate) =
        {
            X = c.X + d.DX
            Y = c.Y + d.DY
        }

    /// make a list of cells the track goes through with the direction from which it enters AND leaves
    let pathToTrackElements (path : Coordinate array) : TrackElement array =
        match path.Length with
        | 0 -> die "empty path!"
        | 1 -> path |> Array.map TrackSingle
        | _ ->
            // both these arrays end up being 1 element shorter due to pairwise comparison
            let entranceDirections = path |> Array.pairwise |> Array.map (fun (curr, next) -> Direction.fromCoordinateDelta next curr)
            let exitDirections = entranceDirections |> Array.map Direction.invert
            let first = TrackTerminator(Array.head path, Array.head exitDirections)
            let last  = TrackTerminator(Array.last path, Array.last entranceDirections)
            let middle =
                let middlePath = path |> Array.skip 1 |> Array.take (path.Length - 2)
                let middleEntrances = entranceDirections |> Array.take (path.Length - 2)
                let middleExits = exitDirections |> Array.skip 1
                Array.zip3 middlePath middleEntrances middleExits |> Array.map (fun (p, n, x) -> TrackMiddle(p, n, x))
            Array.concat [[|first|]; middle; [|last|]]
    
let directionVectors = [ Direction.Left ; Direction.Right ; Direction.Up ; Direction.Down ]

type Sprite = Sprite of char * char * char * char * char * char * char * char * char

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Sprite =
    let makeConstant c = Sprite(c, c, c, c, c, c, c, c, c)
    let make =
        let map0 c =
            Sprite(' ', ' ', ' ', ' ', c, ' ', ' ', ' ', ' ')

        let map1 c =
            let raw =
                [
                    Direction.Up,    Sprite(' ', '|', ' ', ' ', c, ' ', ' ', ' ', ' ')
                    Direction.Down,  Sprite(' ', ' ', ' ', ' ', c, ' ', ' ', '|', ' ')
                    Direction.Left,  Sprite(' ', ' ', ' ', '-', c, ' ', ' ', ' ', ' ')
                    Direction.Right, Sprite(' ', ' ', ' ', ' ', c, '-', ' ', ' ', ' ')
                ]
            let m = raw |> Map.ofList
            fun d -> m |> Map.find d

        let map2 c =
            let raw = 
                [
                    Direction.Up, Direction.Down,    Sprite(' ', '|', ' ', ' ', c, ' ', ' ', '|', ' ')
                    Direction.Up, Direction.Left,    Sprite(' ', '|', ' ', '-', c, ' ', ' ', ' ', ' ')
                    Direction.Up, Direction.Right,   Sprite(' ', '|', ' ', ' ', c, '-', ' ', ' ', ' ')
                    Direction.Down, Direction.Left,  Sprite(' ', ' ', ' ', '-', c, ' ', ' ', '|', ' ')
                    Direction.Down, Direction.Right, Sprite(' ', ' ', ' ', ' ', c, '-', ' ', '|', ' ')
                    Direction.Left, Direction.Right, Sprite(' ', ' ', ' ', '-', c, '-', ' ', ' ', ' ')
                ]
            
            let m = raw |> List.collect (fun (a, b, g) -> [(a, b), g; (b, a), g]) |> Map.ofList
            fun d1 d2 -> m |> Map.find(d1, d2)

        fun c ->
            function
            | TrackSingle(_) ->
                map0 c
            | TrackTerminator(_, d) ->
                map1 c d
            | TrackMiddle(_, d1, d2) ->
                map2 c d1 d2
            
    let rowToStrings (r : Sprite array) =
        let row1 = r |> Array.fold (fun s (Sprite(a, b, c, _, _, _, _, _, _)) -> sprintf "%s%c%c%c" s a b c) ""
        let row2 = r |> Array.fold (fun s (Sprite(_, _, _, d, e, f, _, _, _)) -> sprintf "%s%c%c%c" s d e f) ""
        let row3 = r |> Array.fold (fun s (Sprite(_, _, _, _, _, _, g, h, i)) -> sprintf "%s%c%c%c" s g h i) ""
        [|row1; row2; row3|]

    let gridToStringWithPreamble preamble spriteGrid =
        let rows = spriteGrid |> Array.collect rowToStrings
        Array.concat [preamble; rows] |> Seq.reduce (fun a b -> sprintf "%s%s%s" a Environment.NewLine b)
  

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
    member this.ToTransparentGraphic includeEntry =
        match this, includeEntry with
        | Empty, _ -> None
        | TrainEntry, false -> None
        | _ -> this.ToChar |> Sprite.makeConstant |> Some
    static member EmptyGraphic =
        FixedCell.Empty.ToChar |> Sprite.makeConstant

type PartialCell =
    | Fixed of FixedCell
    | Track

type Board = Board of FixedCell array array
    with
    static member Parse (board : string array) : Board =
        let parseRow (row : string) =
            row.ToCharArray() |> Array.map FixedCell.Parse
        board |> Array.map parseRow |> Board
        
    member this.Height = match this with Board b -> b.Length

    member this.Width = match this with Board b -> b.[0].Length
      
    member this.CoordsOnEdge (coords : Coordinate) =
        [
            coords.X = 0, Direction.Up
            coords.X = this.Height - 1, Direction.Down
            coords.Y = 0, Direction.Left
            coords.Y = this.Width - 1, Direction.Right
        ]
        |> List.choose (fun (b, v) -> if b then Some v else None)
        |> Set.ofList
    
    member this.IsValidSquare (c : Coordinate) =
        let board = match this with Board b -> b
        c.X >= 0 && c.Y >= 0 && c.X < this.Height && c.Y < this.Width

    member this.Find p =
        let arrayfind predicate b =
            let findInRow i = Array.mapi(fun j cell -> predicate cell |> Option.map (fun x -> ({X=i;Y=j}, x))) >> Array.choose id
            b |> Array.mapi findInRow |> Array.concat
        match this with Board b -> b |> arrayfind p
    
    member this.TrainEntry =
        this.Find (function TrainEntry -> Some () | _ -> None) |> Seq.exactlyOne |> fst

    member this.TrainExit =
        this.Find (function TrainExit -> Some () | _ -> None)  |> Seq.exactlyOne |> fst

    member this.TrainEntryOnEdges = this.TrainEntry |> this.CoordsOnEdge

    member this.TrainExitOnEdges = this.TrainExit |> this.CoordsOnEdge

    member this.Aliens =
        this.Find (function Alien(x) -> Some x | _ -> None)

    member this.Boxes =
        this.Find (function Box(x) -> Some x | _ -> None)

    member this.ToTransparentGraphic includeEntry : Sprite option array array =
        match this with Board b -> b |> Array.map (fun row -> row |> Array.map (fun cell -> cell.ToTransparentGraphic includeEntry))

    member this.ToSpriteGridWithOverlay includeEntry overlay =
        let makeNonTransparent c (g : Sprite option) =
            match g, Map.tryFind c overlay with
            | None, None -> Sprite.makeConstant('.')
            | Some q, None -> q
            | None, Some q -> q
            | Some x, Some y -> die <| sprintf "track overlaps with board at %A: %A vs %A" c x y
        this.ToTransparentGraphic includeEntry |> Array.mapi(fun x row -> row |> Array.mapi(fun y cell -> cell |> makeNonTransparent {X=x; Y=y}))


//type Path = (int * int) array

type TrainState =
    {
        Alien : AlienType option
    }

type PartialSolution =
    {
        Board : Board
        Path : (Coordinate * TrainState * int) array
        RemainingAliens : Map<Coordinate,AlienType>
        RemainingBoxes : Map<Coordinate,AlienType>
        PathTouchedEdge : Set<Direction>
        Parent : PartialSolution option
    }

type PartialSolution2 =
    { 
        Board : Board
        Parent : PartialSolution2 option
        NonEmptySegments : Map<Coordinate, AlienType * bool * Coordinate list>
        EmptySegments : Map<Coordinate, Coordinate list>
        UsedSquares : Set<Coordinate>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PartialSolution2 =

    let private tryAddEmptySegment (c : Coordinate) (ps : PartialSolution2) =
        if ps.Board.IsValidSquare c && not (ps.UsedSquares.Contains c) then
            Some { ps with EmptySegments = ps.EmptySegments.Add(c, [c]); UsedSquares = ps.UsedSquares.Add(c) }
        else
            None

    let private tryAddNonEmptySegment (c : Coordinate) (a : AlienType) (ps : PartialSolution2) =
        if ps.Board.IsValidSquare c && not (ps.UsedSquares.Contains c) then
            Some { ps with NonEmptySegments = ps.NonEmptySegments.Add(c, (a, false, [c])); UsedSquares = ps.UsedSquares.Add(c) }
        else
            None

    let private addBoxSegments (c : Coordinate) (ps : PartialSolution2) =
        directionVectors |> List.choose (fun dv -> tryAddEmptySegment (Coordinate.addDirection dv c) ps)

    let private addAlienSegments (c : Coordinate, a : AlienType) (ps : PartialSolution2) : PartialSolution2 list =
        directionVectors |> List.choose (fun dv -> tryAddNonEmptySegment (Coordinate.addDirection dv c) a ps)

    let cartesian l1 l2 =
        l1 |> List.collect (fun i -> l2 |> List.map (fun j -> i, j))

    let make (board : Board) =

        let usedByAliens = board.Aliens |> Seq.map fst |> Set.ofSeq
        let usedByBoxes  = board.Boxes  |> Seq.map fst |> Set.ofSeq

        let raw =
            {
                Board = board
                Parent = None
                NonEmptySegments = Map.empty
                EmptySegments = Map.empty
                UsedSquares = Set.union usedByAliens usedByBoxes |> Set.add board.TrainExit
            } |> tryAddEmptySegment board.TrainEntry |> Option.get

        let withAliens = board.Aliens |> Array.fold (fun list alien -> list |> List.collect (addAlienSegments alien)) [raw]
        let withAliensAndBoxes = board.Boxes |> Array.fold (fun list (c,_) -> list |> List.collect (addBoxSegments c)) withAliens

        withAliensAndBoxes |> List.toArray

    let children (ps : PartialSolution2) : PartialSolution2 array =
        let extendEmptySegmentAt (start : Coordinate) =
            let list = ps.EmptySegments |> Map.find start
            let latest = list |> List.head

            directionVectors
            |> List.map (fun v -> Coordinate.addDirection v latest)
            |> List.filter (ps.UsedSquares.Contains >> not)
            |> List.map (fun c -> { ps with UsedSquares = ps.UsedSquares.Add c ; EmptySegments = ps.EmptySegments.Add (start, c :: list) })
            |> List.toArray
            
        let extendNonEmptySegmentAt (start : Coordinate) =
            let alien, finished, list = ps.NonEmptySegments |> Map.find start
            if finished then die "finished not implemented"
            let latest = list |> List.head

            directionVectors
            |> List.map (fun v -> Coordinate.addDirection v latest)
            |> List.filter (ps.UsedSquares.Contains >> not)
            |> List.map (fun c -> { ps with UsedSquares = ps.UsedSquares.Add c ; NonEmptySegments = ps.NonEmptySegments.Add (start, (alien, finished, c :: list)) })
            |> List.toArray

        let emptySegmentChildren    = ps.EmptySegments    |> Map.toArray |> Array.collect (fst >> extendEmptySegmentAt)
        let nonEmptySegmentChildren = ps.NonEmptySegments |> Map.toArray |> Array.collect (fst >> extendNonEmptySegmentAt)

        Array.append emptySegmentChildren nonEmptySegmentChildren
        
    let toString (ps : PartialSolution2) : string =
        let segmentToSprites c (l : Coordinate list) =
            let path = l |> List.rev |> List.toArray
            let trackElements = path |> Coordinate.pathToTrackElements
            let sprites = trackElements |> Array.map (Sprite.make c)
            Array.zip path sprites |> Map.ofArray

        let emptySegmentToSprites = segmentToSprites 'o'
        let nonEmptySegmentToSprites (AlienType(c), _, l) = segmentToSprites c l

        let mergeMaps = Map.fold (fun m k v -> Map.add k v m)
        let segmentsToSprites converter =
            Map.toSeq >> Seq.map (snd >> converter) >> Seq.fold mergeMaps Map.empty

        let emptySegmentSprites    = ps.EmptySegments    |> segmentsToSprites emptySegmentToSprites
        let nonEmptySegmentSprites = ps.NonEmptySegments |> segmentsToSprites nonEmptySegmentToSprites

        let spriteGrid = ps.Board.ToSpriteGridWithOverlay false (mergeMaps emptySegmentSprites nonEmptySegmentSprites)
        
        Sprite.gridToStringWithPreamble [||] spriteGrid

    let isSolution (ps : PartialSolution2) = false
    
    (*

ideas

segments are keyed by their starting coordinate and contain all other coordinates travelled through in reverse order
in order to connect, the starting coordinate of one must be next to the end coordinate of the other

nonempty segments are bits of track where an alien will be transported
eg if an alien jumps in and right out again the start and end will be the same coordinate and the path will contain one element

empty ones are where the train is moving without an alien

can create child by:
adding a new nonempty segment starting next to a remaining alien
adding a new empty segment starting next to a remaining box
continuing either an empty or nonempty segment

the idea is that it shouldn't be too hard to determine if the segments in progress are logically incompatible
TODO figure this out!


    *)


module Sim =
    let dropAlien (c : Coordinate) (trainState : TrainState) (boxes : Map<Coordinate,AlienType>) =
        match trainState.Alien with
        | None ->
            trainState, boxes
        | Some ts ->
            let rec dropIter dirs =
                match dirs with
                | [] ->
                    trainState, boxes
                | dir::dirs' ->
                    let c' = c |> Coordinate.addDirection dir
                    if boxes.TryFind c' = Some ts
                    then
                        {Alien = None}, boxes.Remove c'
                    else
                        dropIter dirs'

            dropIter directionVectors

    let getAlien (c : Coordinate) (trainState : TrainState) (aliens : Map<Coordinate,AlienType>) =
        match trainState.Alien with
        | Some _ ->
            trainState, aliens
        | None ->
            let rec getIter dirs =
                match dirs with
                | [] ->
                    trainState, aliens
                | dir::dirs' ->
                    let c' = c |> Coordinate.addDirection dir
                    match aliens.TryFind c' with
                    | None ->
                        getIter dirs'
                    | Some a ->
                        {Alien = Some a}, aliens.Remove c'

            getIter directionVectors

    let runTrain c (trainState : TrainState) (aliens : Map<Coordinate,AlienType>) (boxes : Map<Coordinate,AlienType>) =
        let trainState, boxes  = dropAlien c trainState boxes
        let trainState, aliens = getAlien  c trainState aliens
        trainState, aliens, boxes

let mutable totalChildrenMade = 0

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PartialSolution =
    let getBoard (ps : PartialSolution) = ps.Board
    let getPath (ps : PartialSolution) : Coordinate array =
        ps.Path |> Array.map (fun (a, _, _) -> a)
    let getTrainState (ps : PartialSolution) : TrainState =
        ps.Path |> Array.last |> (fun (_, b, _) -> b)
    let getPathWithTrainState (ps : PartialSolution) : (Coordinate * TrainState) array =
        ps.Path |> Array.map (fun (a, b, _) -> (a, b))
    let getTransitionsRemaining (ps : PartialSolution) : int array =
        ps.Path |> Array.map (fun (_, _, c) -> c)
    let getRemainingAliens (ps : PartialSolution) = ps.RemainingAliens
    let getRemainingBoxes (ps : PartialSolution) = ps.RemainingBoxes

    let private makeInner parent (trainState : TrainState) coords (ps : PartialSolution) =
        let trainState, remainingAliens, remainingBoxes = Sim.runTrain coords trainState ps.RemainingAliens ps.RemainingBoxes

        let pathTouchedEdge =
            Set.union (ps.Board.CoordsOnEdge coords) ps.PathTouchedEdge

        {
            Board = ps.Board
            Path = Array.append ps.Path [|coords, trainState, remainingAliens.Count + remainingBoxes.Count|]
            RemainingAliens = remainingAliens
            RemainingBoxes = remainingBoxes
            PathTouchedEdge = pathTouchedEdge
            Parent = parent
        }

    let makeChild coords ps =
        makeInner (Some ps) (getTrainState ps) coords ps

    let makeEmpty (board : Board) =
        {
            Board = board
            Path = Array.empty
            RemainingAliens = board.Aliens |> Map.ofArray
            RemainingBoxes = board.Boxes |> Map.ofArray
            PathTouchedEdge = Set.empty
            Parent = None
        }
        |> makeInner None {Alien = None} board.TrainEntry

    let toSpriteGrid ps =
        let trackElements = ps |> getPath |> Coordinate.pathToTrackElements

        let trackAsGraphics = 
            let annotations = getTransitionsRemaining ps
            let n2c (n : int) = n.ToString() |> Seq.last
    
            match trackElements.Length with
            | 0 ->
                die "empty path"
            | 1 ->
                Map.empty
            | len ->
                trackElements
                |> Array.zip annotations
                |> Array.skip 1 |> Array.take (len - 2)
                |> Array.map (function (n, (TrackMiddle(c, entry, exit) as e)) -> c, (Sprite.make (n2c n) e) | _ -> die "logic error")
                |> Map.ofArray
        //printfn "trackAsGraphics = %A" trackAsGraphics

        let board = getBoard ps
        let overlay = trackAsGraphics

        board.ToSpriteGridWithOverlay true overlay

    let toString ps =
        let preamble =
            [|
            //    sprintf "track: %A" (getPath ps)
            //    sprintf "edges touched: %A" (ps.PathTouchedEdge)
            |]

        ps |> toSpriteGrid |> Sprite.gridToStringWithPreamble preamble

    let mutable debugCounter = 0
    let debug ps =
        debugCounter <- debugCounter + 1
        if debugCounter % 10000 = 0
        then
            printfn "%s" (ps |> toString)
    
    let tryAddTrack c ps =
        match getBoard ps with
        | Board board ->
            if not (ps.Board.IsValidSquare c) then None else
            let ok =
                match board.[c.X].[c.Y] with
                | Empty ->
                    getPath ps |> Array.contains c |> not
                | TrainExit ->
                    true
                | _ ->
                    false
            if ok
            then
                totalChildrenMade <- totalChildrenMade + 1
                Some <| makeChild c ps
            else
                None

    let isComplete ps =
        let c = ps |> getPath |> Array.last
        match getBoard ps with Board(cells) -> cells.[c.X].[c.Y] = TrainExit

    let latestMoveWastesTrack ps = 
        //printfn ""
        //printfn "latestMoveWastesTrack"
        let transitionsRemaining = ps |> getTransitionsRemaining
        //printfn "transitionsRemaining = %A" transitionsRemaining
        let transitionsRemainingAtEnd = transitionsRemaining |> Array.last
        //printfn "transitionsRemainingAtEnd = %A" transitionsRemainingAtEnd
        let c = ps |> getPath |> Array.last
        //printfn "x, y = %A, %A" x y
        let transitionsRemainingMap =
            ps.Path
            |> Array.take (transitionsRemaining.Length - 2)
            |> Array.map (fun (a, _, c) -> a, c)
            |> Map.ofArray
        //printfn "transitionsRemainingMap = %A" transitionsRemainingMap
        let neighbours = 
            directionVectors
            |> List.map (fun d -> Coordinate.addDirection d c)
        //printfn "neighbours = %A" neighbours
        let transitionsRemainingAtNeighbouringTrack =
            neighbours
            |> List.choose transitionsRemainingMap.TryFind
        //printfn "transitionsRemainingAtNeighbouringTrack = %A" transitionsRemainingAtNeighbouringTrack
        let ret = transitionsRemainingAtNeighbouringTrack |> List.exists ((=) transitionsRemainingAtEnd)
        //printfn "ret = %A" ret

        //if ret then printfn "%A" (toString ps); die ""

        ret

    let latestMoveFailsDividedBoard (ps : PartialSolution) =
        match ps.Parent with
        | None -> false
        | Some parent1 ->
            match parent1.Parent with
            | None -> false
            | Some parent2 ->
                let edgesNewlyTouchedByPreviousMove = Set.difference parent1.PathTouchedEdge parent2.PathTouchedEdge
                let newDivision =
                        // Up represents Up-Down axis here and Left represents Left-Right
                    match Set.toList edgesNewlyTouchedByPreviousMove with
                    | [] ->
                        None
                    | [d] when (d = Direction.Up    && parent2.PathTouchedEdge.Contains Direction.Down) ->
                        Some Direction.Up
                    | [d] when (d = Direction.Down  && parent2.PathTouchedEdge.Contains Direction.Up) ->
                        Some Direction.Up
                    | [d] when (d = Direction.Left  && parent2.PathTouchedEdge.Contains Direction.Right) ->
                        Some Direction.Left
                    | [d] when (d = Direction.Right && parent2.PathTouchedEdge.Contains Direction.Left) ->
                        Some Direction.Left
                    | [_] ->
                        None
                    | x ->
                        die <| sprintf "logic error 1: %A" x

                let path = ps |> getPath
                let c1 = path.[path.Length - 1]
                let c2 = path.[path.Length - 2]
                
                let ret =
                    match newDivision with
                    | None -> false
                    | Some d when d = Direction.Left ->
                        (c1.X - c2.X < 0 && ps.Board.TrainExitOnEdges.Contains Direction.Down) ||
                        (c1.X - c2.X > 0 && ps.Board.TrainExitOnEdges.Contains Direction.Up)
                    | Some d when d = Direction.Up ->
                        (c1.Y - c2.Y < 0 && ps.Board.TrainExitOnEdges.Contains Direction.Right) ||
                        (c1.Y - c2.Y > 0 && ps.Board.TrainExitOnEdges.Contains Direction.Left)
                    | _ -> 
                        die "logic error 2"

                ret

    let hasReachableEnd ps =
        let trackset = ps |> getPath |> Set.ofArray

        //printfn "check reachability for board"
        //printfn "%s" (this.ToString())
        let pathEnd = ps |> getPath |> Array.last 
        let rec iter (surrounded : Set<Coordinate>) (todo : Set<Coordinate>) =
        
            if todo.IsEmpty
            then
                // didn't find end
                false
            else
                let first = Set.minElement todo
                let b = match getBoard ps with Board b -> b
                //printfn "first = %A" first
                match first with
                | c when b.[c.X].[c.Y] = TrainExit ->
                    //printfn "%s" "// found end"
                    true
                | h when surrounded.Contains h ->
                    //printfn "%s" "// item item already processed so move on"
                    iter surrounded (Set.remove h todo)
                | h when h <> pathEnd && trackset.Contains h ->
                    //printfn "%s" "// item clashes with track so move on"
                    iter surrounded (Set.remove h todo)
                | c when b.[c.X].[c.Y] = Empty ->
                    //printfn "%s" "// expand neighbours and add this one to surrounded list"
                    let todo' =
                        directionVectors
                        |> List.map (fun d -> Coordinate.addDirection d c)
                        |> List.filter (getBoard ps).IsValidSquare
                        |> List.filter (surrounded.Contains >> not)
                        |> Set.ofList
                        |> Set.union todo
                        |> Set.remove c
                    
                    let surrounded' = Set.add c surrounded
                    iter surrounded' todo'
                | h ->
                    //printfn "%s" "// this item tramples on a bit of board, skip it"
                    iter surrounded (Set.remove h todo)
                       
        let ret = iter Set.empty (Set.add pathEnd Set.empty)

        //printfn "%A" ret
        //if not ret then die "hi"
        ret

    let children ps =
        debug ps
        if isComplete ps then die "can't get children from complete board"
        let c = ps |> getPath |> Array.last
        directionVectors
        |> List.choose (fun d -> ps |> tryAddTrack (Coordinate.addDirection d c))
   //     |> List.filter hasReachableEnd
        |> List.filter (latestMoveFailsDividedBoard >> not)
        |> List.filter (latestMoveWastesTrack >> not)

    let countErrors (ps : PartialSolution) : int =
        let bools =
            [
                isComplete ps |> not
            ]
            |> List.sumBy (function true -> 1 | false -> 0)
        let ints = 
            [
                (getRemainingAliens ps).Count
                (getRemainingBoxes ps).Count
            ]
            |> List.sum
        bools + ints

    let testCompleteSolution (ps : PartialSolution) : bool =
        countErrors ps = 0 && (getTrainState ps).Alien = None

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
            match this, (other :?> 'a Comparable) with
            | Comparable(t, cmp), Comparable(o, _) -> cmp t o
    member this.Value = match this with Comparable (x,_) -> x

    override this.Equals(x) = die ""
    override this.GetHashCode() = die ""

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

type PartialSolutionGameState(ps : PartialSolution) =
    interface ITextBasedAdventureState<PartialSolutionGameState> with
        member this.Parent = ps.Parent |> Option.map PartialSolutionGameState
        member this.Children = PartialSolution.children ps |> List.map PartialSolutionGameState |> List.toArray
        member this.IsSolution = PartialSolution.isComplete ps
    override this.ToString() = PartialSolution.toString ps


type PartialSolution2Wrapper =
    | PartialSolution2Root of Board
    | PartialSolution2 of PartialSolution2

type PartialSolution2GameState(psw : PartialSolution2Wrapper) =
    let convert = PartialSolution2 >> PartialSolution2GameState

    interface ITextBasedAdventureState<PartialSolution2GameState> with
        member this.Parent =
            match psw with
            | PartialSolution2Root(_) -> None
            | PartialSolution2 ps -> ps.Parent |> Option.map convert
        member this.Children = 
            match psw with
            | PartialSolution2Root(board) ->
                PartialSolution2.make board |> Array.map convert
            | PartialSolution2 ps ->
                PartialSolution2.children ps |> Array.map convert
        member this.IsSolution = 
            match psw with
            | PartialSolution2Root(_) -> false
            | PartialSolution2 ps ->
                PartialSolution2.isSolution ps 
    override this.ToString() = 
        match psw with
        | PartialSolution2Root(_) -> "PartialSolution2Root"
        | PartialSolution2 ps -> PartialSolution2.toString ps

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


let initialState = b1 |> Board.Parse |> PartialSolution2Root |> PartialSolution2GameState

initialState |> TextBasedAdventure.run

//partialSolution1 |> PartialSolution.toString |> printfn "%s"

//let soln = shortestPathSolver2 partialSolution1  
//match soln with Some s -> printfn "found solution!"; printfn "%s" (s |> PartialSolution.toString) | None -> printfn "no solution found"


//printfn "total children made: %A" totalChildrenMade



