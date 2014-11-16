module TestBed

open NUnit.Framework
open System

type Location = {
    x: int;
    y: int
}

[<Test>] let ``can run a test``() = 
    Assert.IsTrue(true)

[<Test>] let ``location has coordinates``() = 
    let origin = { x = 0; y = 0; }
    let point = {x = 3; y = 5; }

    Assert.AreEqual(3, point.x)
    Assert.AreEqual(5, point.y)
