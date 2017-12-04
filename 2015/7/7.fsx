open System
open System.IO
open System.Collections.Generic

let cache = Dictionary<string, uint16>()

let isInt str =
    match UInt16.TryParse str with
    | (true, _) -> true
    | (false, _) -> false

let runInstruction operator (arg1, arg2) =
    match operator with
    | "AND" -> arg1 &&& arg2
    | "OR" -> arg1 ||| arg2
    | "LSHIFT" -> arg1 <<< int32(arg2)
    | "RSHIFT" -> arg1 >>> int32(arg2)
    | _ -> failwith "Invalid operator"

let parseLine (line:string) =
    let hyphen = line.IndexOf "-"
    let leftHandParts = line.Substring(0, hyphen).Trim().Split ' '
    let destinationWire = line.Substring(hyphen + 2).Trim()
    (leftHandParts, destinationWire)

let processCommands (commandList:(string[] * string) list) =
    let rec loop commands =
        match commands with
        | [] -> ()
        | h :: t ->
            let leftHandParts, destinationWire = h
            match leftHandParts with
            | [|value|] when isInt value ->
                cache.[destinationWire] <- uint16 value
                loop t
            | [|value|] ->
                let success, v = cache.TryGetValue value
                if success then
                    cache.[destinationWire] <- v
                    loop t
                else
                    loop (t @ [h])
            // only combo is 'NOT wire'
            | [|"NOT"; value|] ->
                let success, v = cache.TryGetValue value
                if success then
                    cache.[destinationWire] <- ~~~ v
                    loop t
                else
                    loop (t @ [h])
            // Ex. 123 LSHIFT 123 -> a
            | [|arg1; instruction; arg2|] when isInt arg1 && isInt arg2 ->
                cache.[destinationWire] <- runInstruction instruction ((uint16 arg1), (uint16 arg2))
                loop t
            // Ex. 123 AND x -> a
            | [|arg1; instruction; arg2|] when isInt arg1 && not (isInt arg2) ->
                let success, v = cache.TryGetValue arg2
                if success then
                    cache.[destinationWire] <- runInstruction instruction ((uint16 arg1), v)
                    loop t
                else
                    loop (t @ [h])
            // Ex. x AND 123 -> a
            | [|arg1; instruction; arg2|] when not (isInt arg1) && isInt arg2 ->
                let success, v = cache.TryGetValue arg1
                if success then
                    cache.[destinationWire] <- runInstruction instruction (v, (uint16 arg2))
                    loop t
                else
                    loop (t @ [h])
            // Ex. x OR c1 -> a
            | [|arg1; instruction; arg2|] ->
                let success, v = cache.TryGetValue arg1
                let success2, v2 = cache.TryGetValue arg2
                if success && success2 then
                    cache.[destinationWire] <- runInstruction instruction (v, v2)
                    loop t
                else
                    loop (t @ [h])
            | _ -> failwith "Invalid instruction pattern"
    loop commandList

let commands =
    File.ReadLines "input2.txt"
    |> Seq.map parseLine
    |> Seq.toList

processCommands commands

printfn "%d" (cache.["a"])
