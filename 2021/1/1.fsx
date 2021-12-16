open System.IO;

let lines = 
    File.ReadLines "input.txt" 
    |> Seq.map (int)
    |> Seq.toList

let part1() =
    let (sum, _) = 
        lines.Tail // skip the first item
        |> Seq.fold (fun (sum, prev) num -> 
            if num > prev then (sum + 1, num) 
            else (sum, num)
        ) (0, 0)
    sum

let part2() =
    let (sum, _) =
        lines.Tail
        |> Seq.windowed 3
        |> Seq.fold (fun (sum, prev) win ->
            let total = Seq.sum win
            if total > prev then (sum + 1, total)
            else (sum, total)
        ) (0, 0)
    sum

printfn "Part 1 = %d" <| part1()
printfn "Part 2 = %d" <| part2()
