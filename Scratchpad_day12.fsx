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

type GardPlot = {Value:string; RegionId:int; Fences:int}

let parseInput (inputLine:string) = inputLine.ToCharArray() |> Array.map (fun v -> {Value = string v; RegionId = -1; Fences = 0})

let findNeighbours (x:int) (y:int) (map: GardPlot array array) =
    let regionValue = map[y].[x].Value
    let neighbours = 
        [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)]
        |> List.filter (fun (x, y) -> 
            x >= 0 && y >= 0 
            && y < map.Length 
            && x < map[y].Length 
            && map[y].[x].Value = regionValue)
    neighbours

let rec exploreRegion (regionId:int) (x:int) (y:int) (map: GardPlot array array) =
    map[y].[x] <- {map.[y].[x] with RegionId = regionId}

    let rec exploreRegion' (coords:(int*int) Set) (map: GardPlot array array) =
            match coords.Count with
            | 0 -> ()
            | _ -> 
                let newCoords =
                    coords 
                    |> Seq.collect (
                        fun (x, y) -> 
                            findNeighbours x y map
                            // |> fun (neighbours:(int*int) list) ->
                            |> fun (neighbours) ->
                                map[y][x] <- {map[y][x] with Fences = 4 - neighbours.Length}
                                match neighbours with
                                | [] -> []
                                | _ -> 
                                    let unvisitedNeighbours = neighbours |> List.filter (fun (x, y) -> map[y].[x].RegionId = -1)
                                    unvisitedNeighbours 
                                        |> List.iter (fun (x, y) -> map[y][x] <- {map[y][x] with RegionId = regionId })
                                    unvisitedNeighbours 
                    )
                    |> Set.ofSeq

                exploreRegion' newCoords map

    
    exploreRegion' ([(x,y)] |> Set.ofList) map


let rec regionFinder (regionId:int) (x:int) (y:int) (map: GardPlot array array) =
    if y = map.Length then
        map
    elif x = map[y].Length then
        regionFinder regionId 0 (y + 1) map
    elif map[y].[x].RegionId <> -1 then
        regionFinder regionId (x + 1) y map
    else
        exploreRegion regionId x y map
        regionFinder (regionId+1) (x + 1) y map

let regionStats (map: GardPlot array array) =
    map 
    |> Array.concat 
    |> Array.groupBy (fun p -> p.RegionId) 
    |> Array.map (fun (regionId, plots) -> (regionId, plots.Length, plots |> Array.sumBy (fun p -> p.Fences)))

// part 1
getInput 12
// getTestInput 12
|> Array.map parseInput
|> regionFinder 0 0 0
|> regionStats
|> Array.sumBy (fun (_, plotCount, fenceCount) -> plotCount * fenceCount)

// part 2
// getInput 12
// getTestInput 12

