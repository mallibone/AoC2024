#time
#r "nuget: FSharp.Collections.ParallelSeq, 1.2.0"
open System.IO
open System
open FSharp.Collections.ParallelSeq

let getTestInput (day:int) =
    let filename = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day:D2}.txt")
    File.ReadAllLines(filename)

let getInput (day:int) =
    let filename = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day:D2}.txt")
    File.ReadAllLines(filename)

type Input = {Result: Int64; Values: Int64 array}

let parseInput (inputLine:string) =
    let splitResult = inputLine.Split(": ")
    {Result = int64 splitResult[0]; Values = splitResult[1].Split(" ") |> Array.map int64}

let validFormulaCount operations input = 
    let rec calculateValues (values:int64 list) (result:int64 list) =
        match values with
        | [] -> result
        | head::tail ->
            let newResult = 
                result 
                |> List.collect (operations head)
                |> List.distinct 
                |> List.filter(fun x -> x <= input.Result )
            calculateValues tail newResult

    calculateValues (input.Values |> Array.toList |> List.tail) [input.Values[0]]
    |> List.filter(fun x -> x = input.Result)

// part 1
getInput 7
// getTestInput 7
|> Array.map parseInput
|> PSeq.collect (validFormulaCount (fun head x -> [x + head; x * head]))
|> PSeq.sum

// part 2
getInput 7
// getTestInput 7
|> Array.map parseInput
|> PSeq.collect (validFormulaCount (fun head x -> [x + head; x * head; (string x + string head) |> int64]))
|> PSeq.sum
