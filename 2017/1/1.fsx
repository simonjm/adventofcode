open System.IO

let toInt (c:char) = int(c.ToString())
let sumChars1 (input:string) =
    let chars = input |> Seq.toList
    let first = chars.Head
    let rec loop sum prev left =
        match left with
        | [] when prev = first -> sum + toInt(first)
        | [] -> sum
        | h :: t when h = prev -> loop (sum + toInt(h)) h t
        | h :: t -> loop sum h t
    loop 0 first chars.Tail

let sumChars2 (input:string) =
    let chars = input |> Seq.toList
    let charArray = input.ToCharArray()
    let step = input.Length / 2
    let rec loop sum i left =
        let matchIndex = if i + step >= input.Length then (i + step) - input.Length else i + step
        match left with
        | [] -> sum
        | h :: t when h = charArray.[matchIndex] -> loop (sum + toInt(h)) (i + 1) t
        | _ :: t -> loop sum (i + 1) t
    loop 0 0 chars
    
// let input = "12131415"
let input = File.ReadAllText "input.txt"

printfn "Part 1 = %d" (sumChars1 input)
printfn "Part 2 = %d" (sumChars2 input)