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

let isRuleValid page rule =
    let ruleId, ruleValues = rule
    let indexOfId = page |> Array.findIndex (fun x -> x = ruleId)
    let indexOfValues = ruleValues |> Array.map(fun r -> page |> Array.tryFindIndex (fun x -> x = r)) |> Array.choose id
    (indexOfValues |> Array.forall(fun v -> v > indexOfId))

let validatePage (rules: (int * int array) array) (page:int array) =
    let rec validatePage' (rules: (int * int array) array) (page:int array) =
        match rules with
        | [||] -> true
        | _ -> 
            if isRuleValid page rules[0] then
                validatePage' rules[1..] page
            else
                false
    let rulesThatApply = rules |> Array.filter (fun (k, v) -> page |> Array.exists (fun x -> x = k))
    validatePage' rulesThatApply page

// part 1

getInput 5
// getTestInput 5
|> parseInput
|> (fun (r,p) -> p |> Array.filter (validatePage r))
|> Array.map(fun x -> x[x.Length / 2])
|> Array.sum

// part 2
let fixInvalidPage (rules: (int * int array) array) (page:int array) =

    let rec fixInvalidPage' page rules =
        match rules with
        | [||] -> page
        | _ -> 
            if isRuleValid page rules[0] then
                fixInvalidPage' page rules.[1..]
            else
                let ruleId, ruleValues = rules[0]
                let indexOfId = page |> Array.findIndex (fun x -> x = ruleId)
                let indexOfValues = ruleValues |> Array.map(fun r -> page |> Array.tryFindIndex (fun x -> x = r)) |> Array.choose id
                let invalidIndex = indexOfValues |> Array.findIndex(fun v -> v <= indexOfId)
                let temp = page.[indexOfId]
                page.[indexOfId] <- page.[indexOfValues.[invalidIndex]]
                page.[indexOfValues.[invalidIndex]] <- temp
                fixInvalidPage' page rules

    let fixedPages = 
        rules 
        |> Array.filter (fun (k, _) -> page |> Array.exists (fun x -> x = k))
        |> Array.filter (isRuleValid page >> not)
        |> (fixInvalidPage' page)

    fixedPages

getInput 5
// getTestInput 5
|> parseInput
|> (fun (r,p) -> r, p |> Array.filter ((validatePage r) >> not))
|> (fun (r,p) -> 
    let rec loop currentPage =
        let fixedPage = fixInvalidPage r (currentPage |> Array.copy)
        if fixedPage = currentPage then 
            fixedPage
        else 
            loop fixedPage
    p |> Array.map loop)
|> Array.map(fun x -> x[x.Length / 2])
|> Array.sum