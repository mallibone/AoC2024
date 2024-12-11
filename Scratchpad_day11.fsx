#time
#r "nuget: FSharp.Collections.ParallelSeq, 1.2.0"
open System.IO

let getTestInput (day:int) =
    let filename = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day:D2}.txt")
    File.ReadAllLines(filename)

let getInput (day:int) =
    let filename = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day:D2}.txt")
    File.ReadAllLines(filename)

let parseInput (inputLine:string) =
    inputLine.Split(" ") |> Array.map (fun s -> int64 s, 1L)

let rec blink roundsRemaining (stones:(int64*int64) seq) =
    if roundsRemaining = 0 then
        stones
    else
        let newStones = 
            stones 
            |> Seq.collect (fun (key, value) ->
                let keyString = string key
                if key = 0L then
                    [1L, value]
                elif keyString.Length % 2 = 0 then
                    let half = keyString.Length / 2
                    let left = int64 (keyString.Substring(0, half))
                    let right = int64 (keyString.Substring(half))
                    [left, value; right, value]
                else
                    [key * 2024L, value]
            )
            |> Seq.groupBy fst
            |> Seq.map (fun (key, values) -> key, Seq.sumBy snd values)

        blink (roundsRemaining - 1) newStones

// part 1
getInput 11
// getTestInput 11
|> Array.head |> parseInput
|> blink 25
|> Seq.sumBy snd

// part 2
getInput 11
// getTestInput 11
|> Array.head |> parseInput
|> blink 75
|> Seq.sumBy snd
