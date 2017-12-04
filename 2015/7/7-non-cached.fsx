open System
open System.IO
open System.Collections.Generic

type WireFunc = unit -> uint16

let wires = Dictionary<string, unit -> uint16>()

let isInt str =
    match UInt16.TryParse str with
    | (true, _) -> true
    | (false, _) -> false

let runInstruction operator (arg1:WireFunc, arg2:WireFunc) destinationWire =
    match operator with
    | "AND" -> wires.[destinationWire] <- (fun _ -> arg1() &&& arg2())
    | "OR" -> wires.[destinationWire] <- (fun _ -> arg1() ||| arg2())
    | "LSHIFT" -> wires.[destinationWire] <- (fun _ -> arg1() <<< int32(arg2()))
    | "RSHIFT" -> wires.[destinationWire] <- (fun _ -> arg1() >>> int32(arg2()))
    | _ -> failwith "Invalid operator"

let parseLine (line:string) =
    let hyphen = line.IndexOf "-"
    let leftHandParts = line.Substring(0, hyphen).Trim().Split ' '
    let destinationWire = line.Substring(hyphen + 2).Trim()
    (*printfn "parts - %A -> %s" leftHandParts destinationWire*)
    match leftHandParts with
    | [|value|] when isInt value -> wires.[destinationWire] <- (fun _ -> uint16 value)
    | [|value|] ->
        wires.[destinationWire] <- (fun _ -> wires.[value]())
    // only combo is 'NOT wire'
    | [|"NOT"; value|] -> 
        wires.[destinationWire] <- (fun _ -> ~~~ (wires.[value]()))
    // Ex. 123 LSHIFT 123 -> a
    | [|arg1; instruction; arg2|] when isInt arg1 && isInt arg2 -> 
        runInstruction instruction ((fun _ -> uint16 arg1), (fun _ -> uint16 arg2)) destinationWire
    // Ex. 123 AND x -> a
    | [|arg1; instruction; arg2|] when isInt arg1 && not (isInt arg2) ->
        runInstruction instruction ((fun _ -> uint16 arg1), (fun _ -> wires.[arg2]())) destinationWire
    // Ex. x AND 123 -> a
    | [|arg1; instruction; arg2|] when not (isInt arg1) && isInt arg2 -> 
        runInstruction instruction ((fun _ -> wires.[arg1]()), (fun _ -> uint16 arg2)) destinationWire
    // Ex. x OR c1 -> a
    | [|arg1; instruction; arg2|] -> 
        runInstruction instruction ((fun _ -> wires.[arg1]()), (fun _ -> wires.[arg2]())) destinationWire
    | _ -> ()

File.ReadLines "input.txt" |> Seq.iter parseLine

printfn "%d" (wires.["a"]())
