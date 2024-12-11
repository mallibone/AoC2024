#time
#r "nuget: FSharp.Collections.ParallelSeq, 1.2.0"
open System.IO

let getTestInput (day:int) =
    let filename = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day:D2}.txt")
    File.ReadAllLines(filename)

let getInput (day:int) =
    let filename = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day:D2}.txt")
    File.ReadAllLines(filename)

let parseInput (inputLines:string array) =
    let map = 
        inputLines
        |> Array.map (fun line -> line.ToCharArray() |> Array.map (string >> int))
    
    let startingPoints =
        [| for x in [0 .. (map[0].Length-1)] do
                for y in [0 .. (map.Length-1)] do 
                    if map[y][x] = 0 then yield (x,y) |]
    
    map, startingPoints

let peakFinder ((map:int array array), (startingpoints:(int*int) array)) =
    let rec peakFinder' (map:int array array) (peaks:Set<(int*int)>) (current:(int*int) Set) =
        if current |> Set.isEmpty then 
            peaks
        else
            let neighbours = 
                current |> Seq.collect (fun (xS,yS) ->
                    [ (xS+1,yS); (xS-1,yS); (xS,yS+1); (xS,yS-1) ]
                    |> List.filter (fun (x,y) -> x >= 0 && y >= 0 && x < map.[0].Length && y < map.Length)
                    |> List.filter (fun (x,y) -> map[y][x] - map[yS][xS] = 1)
                )

            let peaks' = neighbours |> Seq.filter(fun (x,y) -> map[y][x] = 9) |> Seq.toList |> Set.ofList |> Set.union peaks
            let current' = neighbours |> Seq.filter(fun (x,y) -> map[y][x] <> 9) |> Seq.toList |> Set.ofList

            peakFinder' map peaks' current'

    startingpoints
    |> Array.map (fun sp -> peakFinder' map Set.empty ([sp] |> Set.ofList))
    |> Seq.collect id
    |> Seq.length

let trailFinder ((map:int array array), (startingpoints:(int*int) array)) =
    let rec trailFinder' (map:int array array) (trails:int) (current:(int*int) list) =
        match current with
        | [] -> trails
        | _ ->
            let neighbours = 
                current |> Seq.collect (fun (xS,yS) ->
                    [ (xS+1,yS); (xS-1,yS); (xS,yS+1); (xS,yS-1) ]
                    |> List.filter (fun (x,y) -> x >= 0 && y >= 0 && x < map.[0].Length && y < map.Length)
                    |> List.filter (fun (x,y) -> map[y][x] - map[yS][xS] = 1)
                )

            let trails' = (neighbours |> Seq.filter(fun (x,y) -> map[y][x] = 9) |> Seq.length) + trails
            let current' = neighbours |> Seq.filter(fun (x,y) -> map[y][x] <> 9) |> Seq.toList

            trailFinder' map trails' current'
    
    startingpoints 
    |> Seq.map (fun sp -> trailFinder' map 0 [sp]) |> Seq.toList
    |> Seq.sum


// part 1
getInput 10
// getTestInput 10
|> parseInput
|> peakFinder

// part 2
getInput 10
// getTestInput 10
|> parseInput
|> trailFinder
