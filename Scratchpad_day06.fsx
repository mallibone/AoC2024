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

type Direction = 
    | Up
    | Down
    | Left
    | Right


type GameState = {GuardPosition: int * int; GuardDirection:Direction; Map: string array array}

let rec findCoordinatePosition (map: string array array) character =
    map 
    |> Array.mapi (fun y row -> row |> Array.mapi(fun i c -> i, c) |> Array.filter(fun (x, c)-> c = character) |> Array.map(fun (x, c) -> x, y))
    |> Array.concat


let parseInput (inputLine: string array) =
    let map = inputLine |> Array.map (fun x -> x.ToCharArray() |> Array.map string)
    let guardPosition =  findCoordinatePosition map "^" |> Array.head
    map[guardPosition |> snd].[guardPosition |> fst] <- "X"
    {GuardPosition = guardPosition; GuardDirection = Up; Map = map}

let rec moveGuard (gameState: GameState) =
    let (x, y) = gameState.GuardPosition
    let newGuardPosition = 
        match gameState.GuardDirection with
        | Up -> (x, y - 1)
        | Down -> (x, y + 1)
        | Left -> (x - 1, y)
        | Right -> (x + 1, y)

    let (newX, newY) = newGuardPosition
    let maxY = gameState.Map.Length - 1
    let maxX = gameState.Map.[0].Length - 1

    if newX < 0 || newY < 0 || newX > maxX || newY > maxY then
        gameState
    else
        let cell = gameState.Map.[newY].[newX]
        if cell = "#" then
            let newDirection =
                match gameState.GuardDirection with
                | Up -> Right
                | Right -> Down
                | Down -> Left
                | Left -> Up
            moveGuard { gameState with GuardDirection = newDirection }
        else
            let newMap = Array.copy gameState.Map
            newMap.[newY].[newX] <- "X"
            moveGuard { gameState with GuardPosition = newGuardPosition; Map = newMap }

// part 1
getInput 6
// getTestInput 6
|> parseInput
|> moveGuard
|> fun gs -> gs.Map
|> Array.collect (fun m -> m |> Array.filter(fun x -> x = "X"))
|> Array.length

// part 2
// getInput 6
// getTestInput 6

