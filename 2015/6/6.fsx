open System
open System.IO
open System.Text.RegularExpressions

type Command =
    | On
    | Off
    | Toggle

type Instruction = {
    CommandType: Command;
    Start: int * int;
    End: int * int;
}

let instructionRegex = new Regex "(.+?)(\d+),(\d+) through (\d+),(\d+)"
let parseInstruction (line:string) =
    let matches = instructionRegex.Match line
    if not matches.Success then failwith "Something is wrong with the input file"
    let startIndexes = (int(matches.Groups.[2].Value), int(matches.Groups.[3].Value))
    let endIndexes = (int(matches.Groups.[4].Value), int(matches.Groups.[5].Value))
    match matches.Groups.[1].Value.Trim() with
    | "turn on" -> {CommandType = On; Start = startIndexes; End = endIndexes}
    | "turn off" -> {CommandType = Off; Start = startIndexes; End = endIndexes}
    | "toggle" -> {CommandType = Toggle; Start = startIndexes; End = endIndexes}
    | _ -> failwith "Invalid command"

let part1() =
    let grid = Array.init 1000 (fun i -> Array.init 1000 (fun j -> false)) 
    let runInstruction (instruction:Instruction) = 
        let x1, y1 = instruction.Start
        let x2, y2 = instruction.End
        for i = x1 to x2 do
            for j = y1 to y2 do
                match instruction.CommandType with
                | On -> grid.[i].[j] <- true
                | Off -> grid.[i].[j] <- false
                | Toggle -> grid.[i].[j] <- not grid.[i].[j]

    File.ReadLines "input.txt"
    |> Seq.map parseInstruction
    |> Seq.iter runInstruction

    grid |> Array.sumBy (fun i -> Array.sumBy (fun j -> if j then 1 else 0) i) |> printfn "lights on: %d"

let part2() =
    let grid = Array.init 1000 (fun i -> Array.init 1000 (fun j -> 0)) 
    let runInstruction (instruction:Instruction) = 
        let x1, y1 = instruction.Start
        let x2, y2 = instruction.End
        for i = x1 to x2 do
            for j = y1 to y2 do
                match instruction.CommandType with
                | On -> grid.[i].[j] <- grid.[i].[j] + 1
                | Off when grid.[i].[j] > 0 -> grid.[i].[j] <- grid.[i].[j] - 1
                | Toggle -> grid.[i].[j] <- grid.[i].[j] + 2
                | _ -> ()

    File.ReadLines "input.txt"
    |> Seq.map parseInstruction
    |> Seq.iter runInstruction

    grid |> Array.sumBy (fun i -> Array.sum i) |> printfn "total brightness: %d"

part2()


