open System.IO

let runInstructions (instructions:int[]) =
    let values = Array.copy instructions
    let rec loop numMoves i =
        if i < 0 || i >= values.Length then numMoves
        else
            match values.[i] with
            | 0 -> 
                values.[i] <- 1
                loop (numMoves + 1) i
            | n ->
                values.[i] <- n + 1
                loop (numMoves + 1) (i + n)
    loop 0 0

let runInstructions2 (instructions:int[]) =
    let values = Array.copy instructions
    let rec loop numMoves i =
        if i < 0 || i >= values.Length then numMoves
        else
            match values.[i] with
            | 0 -> 
                values.[i] <- 1
                loop (numMoves + 1) i
            | n when n >= 3 ->
                values.[i] <- n - 1
                loop (numMoves + 1) (i + n)
            | n ->
                values.[i] <- n + 1
                loop (numMoves + 1) (i + n)
    loop 0 0

let instructions =
    File.ReadLines "input.txt"
    |> Seq.map int
    |> Seq.toArray

printfn "Part 1 = %d" <| runInstructions instructions
printfn "Part 2 = %d" <| runInstructions2 instructions