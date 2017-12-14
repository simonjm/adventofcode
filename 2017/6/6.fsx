open System
open System.Collections.Generic
open System.IO

let toStr (banks:int[]) = String.Concat banks

let redistribute (banks:int[]) start =
    let len = banks.Length
    let num = banks.[start]
    banks.[start] <- 0
    for i = 1 to num do
        let index = (start + i) % len
        let cur = banks.[index]
        banks.[index] <- cur + 1

let cache = Dictionary<string, int>()
let pickIndex (banks:int[]) =
    let str = toStr banks
    match cache.TryGetValue str with
    | (true, result) -> result
    | (false, _) ->
        let (index, _) = 
            banks
            |> Seq.mapi (fun i n -> (i, n))
            |> Seq.sortWith (fun (i1, n1) (i2, n2) -> 
                let result = n2 - n1 
                if result = 0 then i1 - i2 else result
            )
            |> Seq.head
        cache.Add(str, index)
        index

let hasVisited (visited:HashSet<string>) (banks:int[]) = 
    not <| (visited.Add << toStr) banks

let input = File.ReadAllText("input.txt").Split('\t')

let run banks =
    let visited = hasVisited (HashSet<string>())
    let total =
        Seq.initInfinite id
        |> Seq.filter (fun _ ->
            let start = pickIndex banks
            redistribute banks start
            visited banks
        )
        |> Seq.head
    total + 1

let run2 banks =
    run banks |> ignore
    let startingState = Array.copy banks
    let total =
        Seq.initInfinite id
        |> Seq.filter (fun _ ->
            let start = pickIndex banks
            redistribute banks start
            banks = startingState
        )
        |> Seq.head
    total + 1

let banks = 
    input
    |> Seq.map int
    |> Seq.toArray

printfn "Part 1 = %d" <| (run << Array.copy) banks
printfn "Part 2 = %d" <| (run2 << Array.copy) banks
