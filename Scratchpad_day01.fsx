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
    let filename = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename)
    // File.ReadAllText(filename)

let getInput (day:int) =
    let filename = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
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

let parseInput (input : string array) =
    input
    |> Array.map (fun x -> x.Split("   "))
    |> Array.transpose
    |> Array.map Array.sort
    |> Array.transpose

// part 1
getInput 01
|> parseInput
// getTestInput 01
|> Array.map (fun x -> Math.Abs(int x[0] - int x[1]))
|> Array.sum

// part 2
let computeSimilarity (left : string array) right =
    left |> Array.map (fun x -> (right |> Array.filter (fun y -> x = y) |> Array.length) * int x)
    |> Array.sum
// getInput 1 
getTestInput 1
|> parseInput
|> Array.transpose
|> fun x -> computeSimilarity x[0] x[1]


