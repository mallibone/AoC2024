#time
#r "nuget: FSharp.Collections.ParallelSeq, 1.2.0"
open System.IO
open System.Text.RegularExpressions
open System
open System.Linq
open System.Collections.Generic
open FSharp.Collections.ParallelSeq
open System.Collections.Concurrent

let getTestInput (day:int) =
    let filename = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day:D2}.txt")
    File.ReadAllLines(filename)
    // File.ReadAllText(filename)

let getInput (day:int) =
    let filename = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day:D2}.txt")
    File.ReadAllLines(filename)
    // File.ReadAllText(filename)


// let (|Match|_|) (pat: string) (inp: string) =
//     let m = Regex.Match(inp, pat) in

//     if m.Success then
//         Some(List.tail [ for g in m.Groups -> g.Value ])
//     else
//         None

// let parseInput inputLine =
//     inputLine |> function
//         | Match "Blueprint (.*): Each ore robot costs (.*) ore. Each clay robot costs (.*) ore. Each obsidian robot costs (.*) ore and (.*) clay. Each geode robot costs (.*) ore and (.*) obsidian." [ blueprintId; oreRobotCost; clayRobotCost; obsidianRobotOreCost; obsidianRobotClayCost; geodeRobotOreCost; geodeRobotObsidianCost; ] ->
//             {Id = int blueprintId; OreRobotOreCost = int oreRobotCost; ClayRobotOreCost = int clayRobotCost; ObsidianRobotOreCost = int obsidianRobotOreCost; ObsidianRobotClayCost = int obsidianRobotClayCost; GeodeRobotObsidianCost = int geodeRobotObsidianCost; GeodeRobotOreCost = int geodeRobotOreCost}

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

let findAntiNodes (antennaCoords:Dictionary<string, Tuple<int,int> list>, map:string array array) =
    let maxY = map.Length
    let maxX = map.[0].Length

    let antiNodes = 
        antennaCoords.Values
        |> Seq.collect  (fun (coords:(int*int) list) -> 
            coords 
            |> List.collect (fun (x, y) -> 
                coords 
                |> List.map (fun (x2,y2) -> x2-x, y2-y) 
                |> List.filter (fun (x,y) -> x <> 0 && y <> 0)
                |> List.map (fun (dx,dy) -> x-dx, y-dy)
                |> List.filter (fun (x,y) -> x >= 0 && y >= 0 && x < maxX && y < maxY)
            )
            |> List.distinct
        )
        |> Set.ofSeq

    antiNodes

let drawMap ((map:string array array), (coords:(int*int) list)) =
    coords |> List.iter (fun (x,y) -> map[y][x] <- "#")
    map

// part 1
getInput 8
// getTestInput 8
|> parseInput
|> findAntiNodes
// |> drawMap
|> Seq.length 





// part 2
// getInput 8
// getTestInput 8

