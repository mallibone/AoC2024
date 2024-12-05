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

let parseInput (inputLines:string array) =
    let rules = 
        inputLines |> Array.takeWhile (fun x -> x <> "")
        |> Array.map (fun x -> x.Split("|") |> Array.map int)
        |> Array.sortBy (fun x -> x.[0])
        |> Array.groupBy (fun x -> x.[0])
        |> Array.map (fun (k, v) -> k, v |> Array.collect (fun x -> x |> Array.tail))
    let pages = 
        inputLines |> Array.skipWhile (fun x -> x <> "") |> Array.skip 1
        |> Array.map (fun x -> x.Split(",") |> Array.map int)
    rules, pages

let validatePage (rules: (int * int array) array) (page:int array) =
    let rec validatePage' (rules: (int * int array) array) (page:int array) =
        match rules with
        | [||] -> true
        | _ -> 
            let ruleId, ruleValues = rules[0]
            let indexOfId = page |> Array.findIndex (fun x -> x = ruleId)
            let indexOfValues = ruleValues |> Array.map(fun r -> page |> Array.tryFindIndex (fun x -> x = r)) |> Array.choose id
            if indexOfValues |> Array.forall(fun v -> v > indexOfId) then
                validatePage' rules[1..] page
            else
                false
    let rulesThatApply = rules |> Array.filter (fun (k, v) -> page |> Array.exists (fun x -> x = k))
    validatePage' rulesThatApply page

let findValidPages ((rules: (int * int array) array), pages : (int array array)) =
    pages |> Array.filter (validatePage rules)


// part 1
getInput 5
// getTestInput 5
|> parseInput
|> findValidPages
|> Array.map(fun x -> x[x.Length / 2])
|> Array.sum

// part 2
// getInput 5
// getTestInput 5

