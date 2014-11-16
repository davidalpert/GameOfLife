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

    member this.evolve() =
        new Universe(None)

    member this.maxLocation =
        match this.livingCells.Length with 
        | 0 -> { x = 0; y = 0; }
        | _ -> {
                   x = (this.livingCells |> List.maxBy (fun c -> c.x)).x;
                   y = (this.livingCells |> List.maxBy (fun c -> c.y)).y;
               }

// ---------------------------------------------------------------------------------
// Test helpers

let toS (loc:Location) =
    sprintf "\n%s" ((sprintf "%A" loc).Replace("\n", String.Empty))

let assertAreEqual (expected:obj) (actual:obj) =
    Assert.AreEqual(expected, actual, (sprintf "Expected: %A\n  Actual:   %A\n" expected actual))

let join (separator:string) (lines:string list) =
    String.Join(separator, lines)

let newline = Environment.NewLine

let draw (universe:Universe) =
    let min = { x = 0; y = 0; }
    let max = universe.maxLocation
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
    Console.WriteLine(title)
    Console.WriteLine("----------")
    Console.WriteLine(draw universe)

let validatePicture (expectedLines:string list) (actual:string) =
    let expected = expectedLines |> join newline
    Console.WriteLine("Expected:")
    Console.WriteLine("---------")
    Console.WriteLine(expected)
    Console.WriteLine()
    Console.WriteLine("Actual:")
    Console.WriteLine("-------")
    Console.WriteLine(actual)
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

// The rules:
// ---------
// Any live cell with less than two live neighbours dies, as if caused by under-population.
// Any live cell with two or three live neighbours lives on to the next generation.
// Any live cell with more than three live neighbours dies, as if by overcrowding.
// Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

[<Test>] let ``i) a live cell with no live neighbors dies``() = 
    Assert.Inconclusive("To be written...")

[<Test>] let ``j) a live cell with one live neighbor dies``() = 
    Assert.Inconclusive("To be written...")

[<Test>] let ``k) a live cell with two live neighbors lives``() = 
    Assert.Inconclusive("To be written...")

[<Test>] let ``l) a live cell with three live neighbors lives``() = 
    Assert.Inconclusive("To be written...")

[<Test>] let ``m) a live cell with four live neighbors lives``() = 
    Assert.Inconclusive("To be written...")

[<Test>] let ``n) a dead cell with three live neighbors comes alive``() = 
    Assert.Inconclusive("To be written...")

