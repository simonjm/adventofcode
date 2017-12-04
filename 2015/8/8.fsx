open System
open System.IO
open System.Text.RegularExpressions

let countCharsInMemory (line:string) =
    let rec loop acc charList = 
        match charList with
        | [] -> acc
        | h :: t ->
            match h with
            | '\\' -> 
                match t.Head with
                | '\\' | '"' -> loop (acc + 1) t.Tail
                | 'x' -> loop (acc + 1) t.Tail.Tail.Tail
                | _ -> failwith "Invalid escape sequence"
            | c -> loop (acc + 1) t
    let trimmedLine = line.Trim()
    let chars = trimmedLine.Substring(1, trimmedLine.Length - 2).ToCharArray() |> Array.toList
    if chars.IsEmpty then 0 else loop 0 chars

let rencode (line:string) =
    let trimmedLine = line.Trim()
    let escaped = trimmedLine.Replace("\\", @"\\").Replace("\"", "\\\"")
    escaped

let part1() =
    File.ReadLines "input.txt"
    |> Seq.map (fun line -> (line.Length, countCharsInMemory line))
    |> Seq.reduce (fun (totalCharCount, totalInMemCount) (charCount, inMemCount) -> (totalCharCount + charCount, totalInMemCount + inMemCount))
    |> (fun (totalCharCount, totalInMemCount) -> printfn "%d" (totalCharCount - totalInMemCount))

let part2() =
    File.ReadLines "input.txt"
    |> Seq.map (fun line -> ((rencode line).Length + 2, line.Length))
    |> Seq.reduce (fun (totalCharCount, totalInMemCount) (charCount, inMemCount) -> (totalCharCount + charCount, totalInMemCount + inMemCount))
    |> (fun (totalCharCount, totalInMemCount) -> printfn "%d" (totalCharCount - totalInMemCount))

let test2() =
    File.ReadLines "test.txt"
    |> Seq.map rencode
    |> Seq.iter (printfn "\"%s\"")

part2()

