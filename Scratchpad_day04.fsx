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

let parseInput (inputString:string) =
    inputString.ToCharArray() |> Array.map string

// part 1
let getCandidates (input : string array array) (x:int,y:int) =
    let candidates = 
        [
            [(x,y);(x-1,y);(x-2,y);(x-3,y);];
            [(x,y);(x+1,y);(x+2,y);(x+3,y);];
            [(x,y);(x,y-1);(x,y-2);(x,y-3);];
            [(x,y);(x,y+1);(x,y+2);(x,y+3);];
            [(x,y);(x-1,y-1);(x-2,y-2);(x-3,y-3);];
            [(x,y);(x+1,y+1);(x+2,y+2);(x+3,y+3);];
            [(x,y);(x-1,y+1);(x-2,y+2);(x-3,y+3);];
            [(x,y);(x+1,y-1);(x+2,y-2);(x+3,y-3);];
        ]
        |> List.map (fun c -> c |> List.filter (fun (x,y) -> x >= 0 && y >= 0 && x < input[0].Length && y < input.Length))
        |> List.filter (fun c -> c |> List.length = 4)
        |> List.map (fun coords -> coords |> List.map(fun (x,y)-> input[y][x]) |> fun l -> String.Join("", l), coords)
        |> List.toArray

    candidates

let findXmasOccurences (input : string array array) =
    let xmasCandidateCoords =
        input |> Array.mapi (fun y row -> row |> Array.mapi (fun x cell -> if cell = "X" then (x,y) else (-1,-1)))
        |> Array.collect id
        |> Array.filter (fun (x,y) -> x <> -1)
    
    let xmasOccurences =
        xmasCandidateCoords
        |> Array.collect (fun coord -> getCandidates input coord)
        |> Array.filter (fun x -> fst x = "XMAS")
    
    xmasOccurences
getInput 4
// getTestInput 4
|> Array.map parseInput
|> findXmasOccurences
|> Array.length

// part 2

let getXCandidates (input : string array array) (x:int,y:int) =
    let candidates = 
        [
            [(x-1,y-1);(x,y);(x+3,y+1);];
            [(x+1,y+1);(x,y);(x-1,y+1);];
            [(x-1,y+1);(x,y);(x+1,y+1);];
            [(x+1,y-1);(x,y);(x-1,y-1);];

        ]
        |> List.map (fun c -> c |> List.filter (fun (x,y) -> x >= 0 && y >= 0 && x < input[0].Length && y < input.Length))
        |> List.filter (fun c -> c |> List.length = 4)
        |> List.map (fun coords -> coords |> List.map(fun (x,y)-> input[y][x]) |> fun l -> String.Join("", l), coords)
        // |> List.filter (fun (s,_) -> s = "MAS")
        |> List.toArray

    candidates
let findXXmasOccurences (input:string array array) =
    let xmasCandidateCoords =
        input |> Array.mapi (fun y row -> row |> Array.mapi (fun x cell -> if cell = "A" then (x,y) else (-1,-1)))
        |> Array.collect id
        |> Array.filter (fun (x,y) -> x <> -1)
    
    let masOccurences =
        xmasCandidateCoords
        |> Array.map (fun coord -> getXCandidates input coord)
        // |> Array.filter (fun x -> x |> Array.length = 2)
    masOccurences
// getInput 4

getTestInput 4
|> Array.map parseInput
|> findXXmasOccurences
|> Array.length

