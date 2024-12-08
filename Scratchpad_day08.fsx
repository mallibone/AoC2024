#time
#r "nuget: FSharp.Collections.ParallelSeq, 1.2.0"
open System.IO
open System
open System.Collections.Generic

let getTestInput (day:int) =
    let filename = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day:D2}.txt")
    File.ReadAllLines(filename)

let getInput (day:int) =
    let filename = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day:D2}.txt")
    File.ReadAllLines(filename)

let parseInput (input:string array) =
    let map = input |> Array.map (fun x -> x.ToCharArray() |> Array.map string)
    let antennaCoords = new Dictionary<string, Tuple<int,int> list>()

    map |> Array.iteri (fun y row -> 
        row |> Array.iteri (fun x cell -> 
            if cell <> "." then 
                if antennaCoords.ContainsKey(cell) then antennaCoords.[cell] <- (x, y)::antennaCoords.[cell]
                else antennaCoords.[cell] <- [(x, y)]
        )
    )
    antennaCoords, map

let findAntiNodes antiNodeFunc (antennaCoords:Dictionary<string, Tuple<int,int> list>, map:string array array) =
    let maxY = map.Length
    let maxX = map.[0].Length

    let initAnitNodeFunc = antiNodeFunc maxX maxY

    let antiNodes = 
        antennaCoords.Values
        |> Seq.collect  (fun (coords:(int*int) list) -> 
            coords 
            |> List.collect (fun (x, y) -> 
                coords 
                |> List.map (fun (x2,y2) -> x2-x, y2-y) 
                |> List.filter (fun (dx,dy) -> dx <> 0 && dy <> 0)
                |> List.collect (fun (dx,dy) -> initAnitNodeFunc x y dx dy )
                |> List.filter (fun (x,y) -> x >= 0 && y >= 0 && x < maxX && y < maxY)
            )
            |> List.distinct
        )
        |> Set.ofSeq

    antiNodes

// part 1
getInput 8
// getTestInput 8
|> parseInput
|> findAntiNodes (fun _ _ x y dx dy -> [x-dx, y-dy])
|> Seq.length 

// part 2
let resonantHarmonicAntiNodes (maxX:int) (maxY:int) (x:int) (y:int) (dx:int) (dy:int) =
    let rec loop (x:int) (y:int) antiNodeCoords =
        if x < 0 || y < 0 || x >= maxX || y >= maxY then antiNodeCoords
        else loop (x-dx) (y-dy) ((x,y)::antiNodeCoords)
    loop x y []

getInput 8
// getTestInput 8
|> parseInput
|> findAntiNodes resonantHarmonicAntiNodes
|> Seq.length
