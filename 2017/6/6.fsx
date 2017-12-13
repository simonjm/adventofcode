open System
open System.Collections.Generic
open System.IO

let redistribute (banks:int[]) start =
    let len = banks.Length
    let num = banks.[start]
    banks.[start] <- 0
    for i = 1 to num do
        // printfn "%d -> %A" i banks
        let index = (start + i) % len
        let cur = banks.[index]
        banks.[index] <- cur + 1

let pickIndex (banks:int[]) =
    let result = 
        banks
        |> Seq.mapi (fun i n -> (i, n))
        |> Seq.sortWith (fun (i1, n1) (i2, n2) -> 
            let result = n2 - n1 
            if result = 0 then i1 - i2 else result
        )
        |> Seq.head
    fst result

let visited = HashSet<string>()
let hasVisited (banks:int[]) = 
    let str = String.Concat banks
    not <| visited.Add str

let input = File.ReadAllText("input.txt").Split('\t')

let run banks =
    let total =
        Seq.initInfinite id
        |> Seq.filter (fun _ ->
            let start = pickIndex banks
            redistribute banks start
            hasVisited banks
        )
        |> Seq.head
    total + 1

let banks = 
    input
    |> Seq.map int
    |> Seq.toArray

printfn "Part 1 = %d" <| (run <| Array.copy banks)
