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

let parseMultiplications (inputLine:string) =
    let matches = Regex.Matches(inputLine, @"mul\((\d+),(\d+)\)")
    matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> (int m.Groups.[1].Value * int m.Groups.[2].Value))
    |> Seq.toArray

// part 1
getInput 3
// getTestInput 3
|> Array.collect parseMultiplications
|> Array.sum

// part 2
let removeOperations (inputLine:string) =
    let pattern = @"don't\(\).*?do\(\)"
    Regex.Replace(inputLine, pattern, "gna", RegexOptions.IgnoreCase)
    |> fun x -> Regex.Replace(x, @"don't\(\).*?$", "gna", RegexOptions.IgnoreCase)

getInput 3
// getTestInput 3
|> (fun x -> String.concat "gna" x)
|> removeOperations
|> parseMultiplications
|> Array.sum
