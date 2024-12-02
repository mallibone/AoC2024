#time
#r "nuget: FSharp.Collections.ParallelSeq, 1.2.0"
open System.IO
open System

let getTestInput (day:int) =
    let filename = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename)

let getInput (day:int) =
    let filename = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename)


let applyFilter (input : int array) = 
    (input |> Array.windowed 2 |> Array.exists (fun y -> y[0] = y[1]) |> not)
    && (input |> Array.windowed 2 |> Array.exists (fun y -> Math.Abs(y[0] - y[1]) > 3) |> not)
    && (input |> (fun x -> 
        let diffs = Array.pairwise x |> Array.map (fun (a, b) -> b - a)
        diffs |> Array.forall (fun d -> d > 0) || diffs |> Array.forall (fun d -> d < 0)))

// part 1
getInput 2
// getTestInput 2
|> Array.map (fun x -> x.Split(" ") |> Array.map int)
|> Array.filter applyFilter
|> Array.length


// part 2
let applyDampendFilter (input : int array) =
    if input |> applyFilter then true
    else
        input
        |> Array.mapi (fun i _ -> Array.append (Array.take i input) (Array.skip (i + 1) input))
        |> Array.exists applyFilter

getInput 2
// getTestInput 2
|> Array.map (fun x -> x.Split(" ") |> Array.map int)
|> Array.filter applyDampendFilter
|> Array.length

