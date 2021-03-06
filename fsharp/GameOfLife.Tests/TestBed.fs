﻿module TestBed

open NUnit.Framework
open System

type Location = {
    x: int;
    y: int
}

type CellState =
| Alive
| Dead

type Universe(seed: Location list option) =

    let shouldSurvive (u:Universe) (cell:Location) =
        match u.numberOfNeighborsOf Alive cell with
        | 2
        | 3 -> true
        | _ -> false

    let shouldBeBorn (u:Universe) (cell:Location) =
        match u.numberOfNeighborsOf Alive cell with
        | 3 -> true
        | _ -> false

    member this.livingCells = match seed with
                              | Some(cells) -> cells
                              | None -> []

    member this.stateOf (loc:Location) =
        match this.livingCells |> List.exists (fun cell -> cell = loc) with
        | true -> Alive
        | false -> Dead

    member this.neighborsOf (loc:Location) =
        [
            { x = -1; y = -1 }
            { x =  0; y = -1 }
            { x =  1; y = -1 }
            { x = -1; y =  0 }
            { x =  1; y =  0 }
            { x = -1; y =  1 }
            { x =  0; y =  1 }
            { x =  1; y =  1 }
        ]
        |> List.map (fun n -> { x = loc.x + n.x; y = loc.y + n.y })

    member this.numberOfNeighborsOf (state:CellState) (loc:Location) =
        this.neighborsOf loc
        |> List.filter (fun n -> this.stateOf n = state)
        |> List.length

    member this.evolve() =
        let cellsThatSurvive = 
            this.livingCells |> Seq.filter (shouldSurvive this)

        let cellsThatShouldBeBorn =
            this.livingCells
            |> Seq.collect (this.neighborsOf)
            |> Seq.distinct
            |> Seq.filter (shouldBeBorn this)

        let livingCellsInNextGen = Seq.concat [
                                                  cellsThatSurvive
                                                  cellsThatShouldBeBorn
                                              ]
                                   |> List.ofSeq

        let seed = match livingCellsInNextGen.Length with
                   | 0 -> None
                   | _ -> Some(livingCellsInNextGen)

        new Universe(seed)

// ---------------------------------------------------------------------------------
// Test helpers

let maxLocation (universe:Universe) =
    match universe.livingCells.Length with 
    | 0 -> { x = 0; y = 0; }
    | _ -> {
               x = (universe.livingCells |> List.maxBy (fun c -> c.x)).x;
               y = (universe.livingCells |> List.maxBy (fun c -> c.y)).y;
           }
    
let toS (loc:Location) =
    sprintf "\n%s" ((sprintf "%A" loc).Replace("\n", String.Empty))

let assertAreEqual (expected:obj) (actual:obj) =
    Assert.AreEqual(expected, actual, (sprintf "Expected: %A\n  Actual:   %A\n" expected actual))

let join (separator:string) (lines:string list) =
    String.Join(separator, lines)

let newline = Environment.NewLine

let draw (universe:Universe) =
    let min = { x = 0; y = 0; }
    let max = maxLocation universe
    let maxToDraw = { x = max.x + 1; y = max.y + 1; }
    let lines = 
          seq { for Y in min.y .. maxToDraw.y do
                yield seq { for X in min.x .. maxToDraw.x do
                                   yield match universe.stateOf { x = X; y = Y; } with
                                         | Alive -> "X"
                                         | Dead  -> "."
                          }
                          |> List.ofSeq |> join String.Empty
              }
              |> List.ofSeq |> join newline
    lines

let write (title:string) (universe:Universe) =
    printfn "%s" title
    printfn ""
    printfn "%s" (draw universe)

let validatePicture (expectedLines:string list) (actual:string) =
    let expected = expectedLines |> join newline
    match expected = actual with
    | true -> 
        printfn "%s" actual
    | false ->
        printfn "Expected:"
        printfn "" 
        printfn "%s" expected
        printfn "" 
        printfn "Actual:"
        printfn "" 
        printfn "%s" actual
    Assert.AreEqual(expected, actual)

let validate (expectedLines:string list) (universe:Universe) =
    let picture = draw universe
    validatePicture expectedLines picture

let buildSeedFrom (textPattern:string list) =
    let pattern = 
        textPattern
        |> List.mapi (fun Y line -> 
                        line.ToCharArray()
                        |> Array.mapi (fun X c -> match c with 
                                               | 'X' -> Some({ x = X; y = Y;})
                                               | '.' -> None
                                      )
                        |> List.ofArray
                        |> List.filter (fun c -> c.IsSome)
                        |> List.map (fun c -> c.Value)
                    )
        |> List.collect (fun cells -> cells)

    match pattern.Length with
    | 0 -> None
    | _ -> Some(pattern)

// ---------------------------------------------------------------------------------
// Tests

// The universe of the Game of Life is an infinite two-dimensional orthogonal grid of square cells,...
[<Test>] let ``a) a location in the universe has coordinates``() = 
    let origin = { x = 0; y = 0; }
    let randomPoint = { x = 3; y = 5; }
    Assert.AreEqual(3, randomPoint.x)
    Assert.AreEqual(5, randomPoint.y)

// ...each of which is in one of two possible states, alive or dead.
[<Test>] let ``b) the universe can report on the state of a given cell (addressed by coordinates)``() = 
    let universe = new Universe(None)
    let cell = { x = 3; y = 5; }
    Assert.AreEqual(Dead, universe.stateOf cell)

// Every cell interacts with its eight neighbours, which are the cells 
// that are horizontally, vertically, or diagonally adjacent.
[<Test>] let ``c) the universe can locate the neighbors of a cell ``() = 
    let universe = new Universe(None)
    let cell = { x = 3; y = 5; }
    let neighbors = universe.neighborsOf cell
    let expected = [
                        { x = 2; y = 4; }; { x = 3; y = 4; }; { x = 4; y = 4; };
                        { x = 2; y = 5; }; (* --- cell --- *) { x = 4; y = 5; };
                        { x = 2; y = 6; }; { x = 3; y = 6; }; { x = 4; y = 6; }
                   ]
    CollectionAssert.AreEquivalent(expected |> List.map toS, neighbors |> List.map toS)

[<Test>] let ``d) the universe can be seeded with a pattern of living cells``() = 
    let pattern = [
                        { x = 3; y = 3; }; { x = 4; y = 3; }; { x = 5; y = 3; }
                  ]

    let seed = Some(pattern)

    let universe = new Universe(seed)

    assertAreEqual Alive (universe.stateOf { x = 3; y = 3; })
    assertAreEqual Alive (universe.stateOf { x = 4; y = 3; })
    assertAreEqual Alive (universe.stateOf { x = 5; y = 3; })

// The first generation is created by applying the above rules 
// simultaneously to every cell in the seed—births and deaths 
// occur simultaneously, and the discrete moment at which this 
// happens is sometimes called a tick (in other words, each 
// generation is a pure function of the preceding one).
[<Test>] let ``e) the universe can evolve``() = 
    let universe = new Universe(None)
    let nextGen = universe.evolve()
    Assert.IsInstanceOf(universe.GetType(), nextGen)
    Assert.AreNotSame(universe, nextGen)

[<Test>] let ``f) can draw an empty universe``() = 
    let universe = new Universe(None)
    let picture = draw universe
    let expected = [ // 01
                       ".." // 0
                       ".." // 1
                   ]
    validatePicture expected picture 
    
[<Test>] let ``g) can draw a universe with living cells``() = 
    let pattern = [
                        { x = 3; y = 3; }; { x = 4; y = 3; }; { x = 5; y = 3; }
                  ]

    let seed = Some(pattern)

    let universe = new Universe(seed)

    let expected = [ // 0123456
                       "......." // 0
                       "......." // 1
                       "......." // 2
                       "...XXX." // 3
                       "......." // 4
                   ]

    validate expected universe 

[<Test>] let ``h) can seed a universe with a text pattern``() = 
    let pattern = [ // 01234
                      "....." // 0
                      ".XXX." // 1
                      "....." // 2
                   ]

    let seed = buildSeedFrom pattern

    let universe = new Universe(seed)

    validate pattern universe

[<Test>] let ``i) a universe can count the number of living neighbors``() = 
    let pattern = [ // 01234
                      "....." // 0
                      "..XX." // 1
                      "...X." // 2
                   ]

    let seed = buildSeedFrom pattern

    let universe = new Universe(seed)

    Assert.AreEqual(0, universe.numberOfNeighborsOf Alive { x = 0; y = 0; })
    Assert.AreEqual(1, universe.numberOfNeighborsOf Alive { x = 1; y = 1; })
    Assert.AreEqual(2, universe.numberOfNeighborsOf Alive { x = 2; y = 1; })
    Assert.AreEqual(3, universe.numberOfNeighborsOf Alive { x = 2; y = 2; })
    Assert.AreEqual(2, universe.numberOfNeighborsOf Alive { x = 3; y = 1; })

// The rules:
// ---------
// Any live cell with less than two live neighbours dies, as if caused by under-population.
// Any live cell with two or three live neighbours lives on to the next generation.
// Any live cell with more than three live neighbours dies, as if by overcrowding.
// Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

[<Test>] let ``j) a live cell with no live neighbors dies``() = 
    let pattern =  [ // 01234
                       "....." // 0
                       "..X.." // 1
                       "....." // 2
                   ]

    let expected = [ // 012
                       ".." // 0
                       ".." // 1
                   ]

    let seed = buildSeedFrom pattern

    let universe = new Universe(seed)

    let nextGen = universe.evolve()

    write "Seed" universe
    validate expected nextGen

[<Test>] let ``k) a live cell with one live neighbor dies``() = 
    let pattern =  [ // 01234
                       "....." // 0
                       ".XX.." // 1
                       "....." // 2
                   ]

    let expected = [ // 012
                       ".." // 0
                       ".." // 1
                   ]

    let seed = buildSeedFrom pattern

    let universe = new Universe(seed)

    let nextGen = universe.evolve()

    write "Seed" universe
    validate expected nextGen

[<Test>] let ``l) a live cell with two live neighbors lives``() = 
    let pattern =  [ // 01234
                       "....." // 0
                       ".XXX." // 1
                       "....." // 2
                   ]

    let expected = [ // 0123
                       "..X." // 0
                       "..X." // 1
                       "..X." // 2
                       "...." // 3
                   ]

    let seed = buildSeedFrom pattern

    let universe = new Universe(seed)

    let nextGen = universe.evolve()

    write "Seed" universe
    validate expected nextGen

[<Test>] let ``m) a live cell with three live neighbors lives``() = 
    let pattern =  [ // 01234
                       "....." // 0
                       ".XXX." // 1
                       "..X.." // 2
                       "....." // 3
                   ]

    let expected = [ // 01234
                       "..X.." // 0
                       ".XXX." // 1
                       ".XXX." // 2
                       "....." // 3
                   ]

    let seed = buildSeedFrom pattern

    let universe = new Universe(seed)

    let nextGen = universe.evolve()

    write "Seed" universe
    validate expected nextGen

[<Test>] let ``n) a live cell with four live neighbors dies``() = 
    let pattern =  [ // 01234
                       "....." // 0
                       "..X.." // 1
                       ".XXX." // 2
                       "..X.." // 3
                       "....." // 4
                   ]

    let expected = [ // 01234
                       "....." // 0
                       ".XXX." // 1
                       ".X.X." // 2
                       ".XXX." // 3
                       "....." // 4
                   ]

    let seed = buildSeedFrom pattern

    let universe = new Universe(seed)

    let nextGen = universe.evolve()

    write "Seed" universe
    validate expected nextGen

[<Test>] let ``o) a dead cell with three live neighbors comes alive``() = 
    let pattern =  [ // 01234
                       "....." // 0
                       "..X.." // 1
                       ".XXX." // 2
                       "..X.." // 3
                       "....." // 4
                   ]

    let expected = [ // 01234
                       "....." // 0
                       ".XXX." // 1
                       ".X.X." // 2
                       ".XXX." // 3
                       "....." // 4
                   ]

    let seed = buildSeedFrom pattern

    let universe = new Universe(seed)

    let nextGen = universe.evolve()

    write "Seed" universe
    validate expected nextGen

[<Test>] let ``p) still lifes``() = 
    let pattern =  [ // 01234567890123
                       ".............." // 0
                       "..XX......XX.." // 1
                       "..XX.....X..X." // 2
                       "..........X.X." // 3
                       "...........X.." // 4
                       "...XX........." // 5
                       "..X..X........" // 6
                       "...XX....XX..." // 7
                       ".........X.X.." // 8
                       "..........X..." // 9
                       ".............." // 0
                   ]

    let expected = pattern

    let seed = buildSeedFrom pattern

    let universe = new Universe(seed)

    let nextGen = universe.evolve()

    write "Seed" universe
    validate expected nextGen

[<Test>] let ``q) oscillators (period 2)``() = 
    let pattern =  [ // 01234567890123
                       ".............." // 0
                       ".............." // 1
                       "..XXX.....XXX." // 2
                       ".........XXX.." // 3
                       ".............." // 4
                       ".............." // 5
                       "...XX........." // 6
                       "...XX........." // 7
                       ".....XX......." // 8
                       ".....XX......." // 9
                       ".............." // 0
                   ]

    let expected = [ // 01234567890123
                       ".............." // 0
                       "...X.......X.." // 1
                       "...X.....X..X." // 2
                       "...X.....X..X." // 3
                       "..........X..." // 4
                       ".............." // 5
                       "...XX........." // 6
                       "...X.........." // 7
                       "......X......." // 8
                       ".....XX......." // 9
                       ".............." // 0
                   ]

    let seed = buildSeedFrom pattern
    let universe = new Universe(seed)
    write "Seed" universe

    printfn ""
    printfn "Generation 1:"
    printfn ""

    let gen1 = universe.evolve()
    validate expected gen1

    printfn ""
    printfn "Generation 2:"
    printfn ""
    let gen2 = gen1.evolve()
    validate pattern gen2

[<Test>] let ``r) oscillators (period 3)``() = 
    let pattern =   [ // 0123456789012345
                        "................" // 0
                        "................" // 1
                        "....XXX...XXX..." // 2
                        "................" // 3
                        "..X....X.X....X." // 4
                        "..X....X.X....X." // 5
                        "..X....X.X....X." // 6
                        "....XXX...XXX..." // 7
                        "................" // 8
                        "....XXX...XXX..." // 9
                        "..X....X.X....X." // 0
                        "..X....X.X....X." // 1
                        "..X....X.X....X." // 2
                        "................" // 3
                        "....XXX...XXX..." // 4
                        "................" // 5
                    ]

    let expected1 = [ // 01234567890123456
                        "................." // 0
                        ".....X.....X....." // 1
                        ".....X.....X....." // 2
                        ".....XX...XX....." // 3
                        "................." // 4
                        ".XXX..XX.XX..XXX." // 5
                        "...X.X.X.X.X.X..." // 6
                        ".....XX...XX....." // 7
                        "................." // 8
                        ".....XX...XX....." // 9
                        "...X.X.X.X.X.X..." // 0
                        ".XXX..XX.XX..XXX." // 1
                        "................." // 2
                        ".....XX...XX....." // 3
                        ".....X.....X....." // 4
                        ".....X.....X....." // 5
                        "................." // 6
                   ]

    let expected2 = [ // 01234567890123456
                        "................" // 0
                        "................" // 1
                        "....XX.....XX..." // 2
                        ".....XX...XX...." // 3
                        "..X..X.X.X.X..X." // 4
                        "..XXX.XX.XX.XXX." // 5
                        "...X.X.X.X.X.X.." // 6
                        "....XXX...XXX..." // 7
                        "................" // 8
                        "....XXX...XXX..." // 9
                        "...X.X.X.X.X.X.." // 0
                        "..XXX.XX.XX.XXX." // 1
                        "..X..X.X.X.X..X." // 2
                        ".....XX...XX...." // 3
                        "....XX.....XX..." // 4
                        "................" // 5
                   ]

    let seed = buildSeedFrom pattern
    let universe = new Universe(seed)
    write "Seed" universe

    printfn ""
    printfn "Generation 1:"
    printfn ""

    let gen1 = universe.evolve()
    validate expected1 gen1

    printfn ""
    printfn "Generation 2:"
    printfn ""

    let gen2 = gen1.evolve()
    validate expected2 gen2

    printfn ""
    printfn "Generation 3:"
    printfn ""

    let gen3 = gen2.evolve()
    validate pattern gen3

[<Test>] let ``s) spaceships``() = 
    let pattern =   [ // 0123456
                        "......." // 0
                        "..X...." // 1
                        "...X..." // 2
                        ".XXX..." // 3
                        "......." // 4
                        "......." // 5
                        "......." // 6
                        "......." // 7
                        "......." // 8
                        "..XXXX." // 9
                        ".X...X." // 0
                        ".....X." // 1
                        ".X..X.." // 2
                        "......." // 3
                    ]

    let expected1 = [ // 01234567
                        "........"
                        "........"
                        ".X.X...."
                        "..XX...."
                        "..X....."
                        "........"
                        "........"
                        "........"
                        "...XX..."
                        "..XXXX.."
                        "..XX.XX."
                        "....XX.."
                        "........"
                   ]

    let expected2 = [ // 01234567
                        "........"
                        "........"
                        "...X...."
                        ".X.X...."
                        "..XX...."
                        "........"
                        "........"
                        "........"
                        "..X..X.."
                        "......X."
                        "..X...X."
                        "...XXXX."
                        "........"
                     ]

    let expected3 = [ // 012345678
                        "........."
                        "........."
                        "..X......"
                        "...XX...."
                        "..XX....."
                        "........."
                        "........."
                        "........."
                        "........."
                        ".....XX.."
                        "...XX.XX."
                        "...XXXX.."
                        "....XX..."
                        "........."
                   ]

    let expected4 = [ // 012345678
                        "........."
                        "........."
                        "...X....."
                        "....X...."
                        "..XXX...."
                        "........."
                        "........."
                        "........."
                        "........."
                        "....XXXX."
                        "...X...X."
                        ".......X."
                        "...X..X.."
                        "........."
                   ]

    let expected5 = [ // 0123456789
                        ".........."
                        ".........."
                        ".........."
                        "..X.X....."
                        "...XX....."
                        "...X......"
                        ".........."
                        ".........."
                        ".....XX..."
                        "....XXXX.."
                        "....XX.XX."
                        "......XX.."
                        ".........."
                   ]

    let expected6 = [ // 0123456789
                        ".........."
                        ".........."
                        ".........."
                        "....X....."
                        "..X.X....."
                        "...XX....."
                        ".........."
                        ".........."
                        "....X..X.."
                        "........X."
                        "....X...X."
                        ".....XXXX."
                        ".........."
                   ]

    let seed = buildSeedFrom pattern
    let universe = new Universe(seed)
    write "Seed" universe

    printfn ""
    printfn "Generation 1:"
    printfn ""

    let gen1 = universe.evolve()
    validate expected1 gen1

    printfn ""
    printfn "Generation 2:"
    printfn ""

    let gen2 = gen1.evolve()
    validate expected2 gen2

    printfn ""
    printfn "Generation 3:"
    printfn ""

    let gen3 = gen2.evolve()
    validate expected3 gen3

    printfn ""
    printfn "Generation 4:"
    printfn ""

    let gen4 = gen3.evolve()
    validate expected4 gen4

    printfn ""
    printfn "Generation 5:"
    printfn ""

    let gen5 = gen4.evolve()
    validate expected5 gen5

    printfn ""
    printfn "Generation 6:"
    printfn ""

    let gen6 = gen5.evolve()
    validate expected6 gen6
