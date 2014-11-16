module TestBed

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

// ---------------------------------------------------------------------------------
// Test helpers

let toS (loc:Location) =
    sprintf "\n%s" ((sprintf "%A" loc).Replace("\n", String.Empty))

let assertAreEqual (expected:obj) (actual:obj) =
    Assert.AreEqual(expected, actual, (sprintf "Expected: %A\n  Actual:   %A\n" expected actual))

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
