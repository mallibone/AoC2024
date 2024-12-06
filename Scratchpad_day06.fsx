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

type GameState = {GuardPosition: int * int; GuardDirection:Direction; Map: string array array; VisitedPositions : ((int * int) * Direction) Set; IsLoop: bool}

let rec findCoordinatePosition (map: string array array) character =
    map 
    |> Array.mapi (fun y row -> row |> Array.mapi(fun i c -> i, c) |> Array.filter(fun (x, c)-> c = character) |> Array.map(fun (x, c) -> x, y))
    |> Array.concat


let parseInput (inputLine: string array) =
    let map = inputLine |> Array.map (fun x -> x.ToCharArray() |> Array.map string)
    let guardPosition =  findCoordinatePosition map "^" |> Array.head
    // map[guardPosition |> snd].[guardPosition |> fst] <- "X"
    {GuardPosition = guardPosition; GuardDirection = Up; Map = map; VisitedPositions = [(guardPosition, Up)] |> Set.ofList; IsLoop = false}

let changeDirection currentDirection =
    match currentDirection with
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up


let getNextGuardPositionx (gameState: GameState) =
        let (x, y) = gameState.GuardPosition
        match gameState.GuardDirection with
        | Up -> (x, y - 1)
        | Down -> (x, y + 1)
        | Left -> (x - 1, y)
        | Right -> (x + 1, y)

let rec moveGuard (gameState: GameState) =
    let newGuardPosition = getNextGuardPositionx gameState

    let (newX, newY) = newGuardPosition
    let maxY = gameState.Map.Length - 1
    let maxX = gameState.Map.[0].Length - 1

    if newX < 0 || newY < 0 || newX > maxX || newY > maxY then
        gameState
    elif gameState.VisitedPositions |> Seq.exists (fun (position, direction) -> position = newGuardPosition && direction = gameState.GuardDirection) then
        { gameState with IsLoop = true }
    else
        let cell = gameState.Map.[newY].[newX]
        if cell = "#" then
            let newDirection = changeDirection gameState.GuardDirection
            moveGuard { gameState with GuardDirection = newDirection }
        else
            let newMap = Array.copy gameState.Map
            moveGuard { gameState with GuardPosition = newGuardPosition; Map = newMap; VisitedPositions = gameState.VisitedPositions.Add(newGuardPosition, gameState.GuardDirection) }

// part 1
getInput 6
// getTestInput 6
|> parseInput
|> moveGuard
|> fun gs -> gs.VisitedPositions
|> Seq.map (fun (x, _) -> x) |> Set.ofSeq
|> Seq.length

// part 2
let findGuardLoops (gameState:GameState) =
    let initialGuardPos = findCoordinatePosition gameState.Map "^" |> Array.head
    gameState.VisitedPositions |> Seq.tail |> Seq.map (fun (pos, _) -> pos) |> Set.ofSeq
    |> Set.map (fun (x,y) -> 
        let map' = gameState.Map |> Array.map Array.copy
        map'[y][x] <- "#"
        {GuardPosition = initialGuardPos; GuardDirection = Up; Map = map'; VisitedPositions = [initialGuardPos, Up] |> Set.ofList; IsLoop = false})
    |> PSeq.map moveGuard
    |> PSeq.filter (fun gs -> gs.IsLoop)
    |> PSeq.toList

getInput 6
// getTestInput 6
|> parseInput
|> moveGuard
|> findGuardLoops
|> List.length

