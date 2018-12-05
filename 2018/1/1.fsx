open System.IO;
open System.Collections.Generic;
open System.Collections.Generic
open System.Data

type Op =
    | Inc of int
    | Dec of int

let parseLine (line:string) =
    let num = int(line.Substring(1))
    match line.[0] with
    | '+' -> Inc num
    | '-' -> Dec num
    | x -> failwithf "Invalid op %c" x

let execOp total op =
    match op with
    | Inc num -> total + num 
    | Dec num -> total - num

let lines = File.ReadLines "input.txt" |> Seq.toList

let part1() =
    lines
    |> Seq.map parseLine
    |> Seq.fold execOp 0

let part2() =
    let set = HashSet<int>()
    let rec loop list acc =
        match list with
        | [] -> loop lines acc
        | h :: t -> 
            let total = parseLine h |> execOp acc
            if set.Add(total) then loop t total
            else total
    loop lines 0

part1() |> printfn "Part 1 = %d"
part2() |> printfn "Part 2 = %d"
