open System.IO
open System

let isValid (parts:seq<string>) =
    let hasDuplicate =
        parts
        |> Seq.groupBy id
        |> Seq.exists (fun (_, v) -> Seq.length v > 1)
    not hasDuplicate

let isValid2 (parts:seq<string>) =
    let sorted =
        parts |> Seq.map (Seq.sort >> String.Concat)
    isValid sorted

let numValid f =
    File.ReadLines "input.txt"
    |> Seq.map (fun l -> l.Split(' '))
    |> Seq.filter f
    |> Seq.length

let part1() =
    let numValid = numValid isValid
    printfn "Part 1 = %d" numValid

let part2() =
    let numValid = numValid isValid2
    printfn "Part 2 = %d" numValid

part1()
part2()