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

// let paresInput (inputLines:string array) =
let parseInput (inputLines:string array) =
    let map = 
        inputLines
        |> Array.map (fun line -> line.ToCharArray() |> Array.map (string >> int))
    
    let startingPoints =
        [| for x in [0 .. (map[0].Length-1)] do
                for y in [0 .. (map.Length-1)] do 
                    if map[y][x] = 0 then yield (x,y) |]
    
    map, startingPoints

let rec peakFinder (map:int array array) (visited:Set<(int*int)>) (current:(int*int) Set) =
    if current |> Set.isEmpty then 
        visited
    else
        let neighbours = 
            current |> Seq.collect (fun (xS,yS) ->
                [ (xS+1,yS); (xS-1,yS); (xS,yS+1); (xS,yS-1) ]
                |> List.filter (fun (x,y) -> x >= 0 && y >= 0 && x < map.[0].Length && y < map.Length)
                |> List.filter (fun (x,y) -> map[y][x] - map[yS][xS] = 1)
            )

        let visited' = neighbours |> Seq.filter(fun (x,y) -> map[y][x] = 9) |> Seq.toList |> Set.ofList |> Set.union visited
        let current' = neighbours |> Seq.filter(fun (x,y) -> map[y][x] <> 9) |> Seq.toList |> Set.ofList

        peakFinder map visited' current'

let trailScores ((map:int array array), (startingpoints:(int*int) array)) =
    startingpoints
    |> Array.map (fun sp -> peakFinder map Set.empty ([sp] |> Set.ofList))
    |> Seq.collect id

// part 1
getInput 10
// getTestInput 10
|> parseInput
|> trailScores
|> Seq.length

// part 2
// getInput 10
// getTestInput 10

