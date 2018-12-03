open System.IO;

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

let lines = File.ReadAllLines "input.txt"

let processLines start =
    lines
    |> Seq.map parseLine
    |> Seq.fold execOp start

processLines 0 |> printfn "Part 1 = %d"
