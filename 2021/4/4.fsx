open System;
open System.IO;

let boardSize = 5

let (input, boards) = 
    let lines = File.ReadLines "input.txt" |> Seq.toList
    let input = lines.Head.Split "," |> Seq.map int |> Seq.toList
    let boards = 
        lines
        |> Seq.skip 2
        |> Seq.filter (fun line -> line.Length = 14) // filter out lines that don't contain a board row
        |> Seq.chunkBySize boardSize
        |> Seq.map (fun chunk -> Array2D.init boardSize boardSize (fun i j -> 
                let row = chunk.[i].Split(" ") |> Array.filter (fun v -> v.Length > 0)
                row.[j]
            )
        )
        |> Seq.toList
    (input, boards)

let part1() =
    let test = boards |> Seq.head
    let i = 1
    printfn $"{test.[i, 1]}"

part1()
// let part2() =

// printfn "Part 1 = %d" <| part1()
// printfn "Part 2 = %d" <| part2()
