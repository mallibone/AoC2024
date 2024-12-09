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

let getInput (day:int) =
    let filename = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day:D2}.txt")
    File.ReadAllLines(filename)

let createFileAndFreeSpace index fileInput =
    match fileInput with
    | [|size; freeSpace|] -> List.init size (fun _ -> int64 index) @ List.init freeSpace (fun _ -> int64 -1)
    | [|size|] -> List.init size (fun _ -> int64 index)
    | _ -> failwith "Invalid input"

let parseInput (inputLine:string) =
    inputLine.ToCharArray() |> Array.map string |> Array.map int
    |> Array.chunkBySize 2
    |> Array.mapi createFileAndFreeSpace
    |> Array.map List.toArray

let rec findLastValueIndex index (diskSpace:int64 array) =
    if diskSpace[index] = -1 then
        findLastValueIndex (index - 1) diskSpace
    else
        index

let rec compressValues index (diskSpace:int64 array)  =
    if index = diskSpace.Length then
        diskSpace
    else
        let value = diskSpace[index]
        if value = -1 then
            let nextIndex = findLastValueIndex (diskSpace.Length - 1) diskSpace
            if nextIndex < index then
                diskSpace
            else
                diskSpace[index] <- diskSpace[nextIndex]
                diskSpace[nextIndex] <- -1
                compressValues (index + 1) diskSpace
        else
            compressValues (index + 1) diskSpace

let checksum (diskSpace:int64 array) =
    diskSpace
    |> Seq.mapi (fun i x -> if x <> -1 then int64 i * x else 0)
    |> Seq.sum

// part 1
// getInput 9
getTestInput 9
|> Array.head
|> parseInput
|> Array.concat
|> compressValues 0
|> checksum

// part 2
let rec findLastFileIndex index (diskSpace:int64 array array) =
    if diskSpace[index][0] = -1 then
        findLastFileIndex (index - 1) diskSpace
    else
        index

let rec findFirstFileWithFreeSpace index (diskSpace:int64 array array) (fileSize:int) =
    let freeSpaceAtIndex = diskSpace[index] |> Array.filter ((=) -1) |> Array.length
    if  freeSpaceAtIndex >= fileSize  then
        index, diskSpace[index].Length - freeSpaceAtIndex
    else
        if index = diskSpace.Length - 1 then
            index+1, -1
        else
            findFirstFileWithFreeSpace (index + 1) diskSpace fileSize

let compressFiles (diskSpace:int64 array array) =
    let rec compressFiles' index (diskSpace:int64 array array) =
        if index < 0 then
            diskSpace
        else
            let file = diskSpace[index]
            if file[0] = -1 then
                compressFiles' (index - 1) diskSpace
            else

                let fileSize = (file |> Array.filter ((=) file[0]) |> Array.length)
                let targetIndex, freeSpaceIndex = findFirstFileWithFreeSpace 0 diskSpace fileSize
                if targetIndex > index then
                    compressFiles' (index - 1) diskSpace
                else
                    diskSpace[targetIndex][freeSpaceIndex .. freeSpaceIndex+(fileSize-1)] <- diskSpace[index][0 .. fileSize-1]
                    diskSpace[index][0 .. fileSize-1] <- Array.init (fileSize) (fun _ -> -1)
                    compressFiles' (index - 1) diskSpace
    compressFiles' (diskSpace.Length-1) diskSpace

getInput 9
// getTestInput 9
|> Array.head
|> parseInput
|> compressFiles
|> Array.concat
|> checksum
