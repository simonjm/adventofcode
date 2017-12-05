open System.IO

let isValid (parts:seq<string>) =
    let hasDuplicate =
        parts
        |> Seq.groupBy id
        |> Seq.exists (fun (_, v) -> Seq.length v > 1)
    not hasDuplicate

let part1() =
    let numValid =
        File.ReadLines "input.txt"
        |> Seq.map (fun l -> l.Split(' '))
        |> Seq.filter isValid
        |> Seq.length
    printfn "Part 1 = %d" numValid

part1()