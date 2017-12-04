open System
open System.IO

let splitLine (l:string) = l.Split([|' '; '\t'|])
let numsToInts (nums:string[]) = nums |> Seq.map int
let minMax (nums:seq<int>) =
    Seq.fold (fun (max, min) n -> 
        let newMax = if n > max then n else max
        let newMin = if n < min then n else min
        (newMax, newMin)
    ) (Int32.MinValue, Int32.MaxValue) nums

let evenlyDivisible (nums:seq<int>) =
    let (n1, n2, _) =
        nums
        |> Seq.collect (fun i -> Seq.map (fun j -> (i, j, i % j)) nums)
        |> Seq.filter (fun (n1, n2, result) -> n1 <> n2 && result = 0)
        |> Seq.head
    n1 / n2

let part1() =
    // File.ReadLines "test.txt"
    File.ReadLines "input.txt"
    |> Seq.map (splitLine >> numsToInts >> minMax)
    |> Seq.sumBy (fun (max, min) -> max - min)

let part2() = 
    // File.ReadLines "test2.txt"
    File.ReadLines "input.txt"
    |> Seq.map (splitLine >> numsToInts >> evenlyDivisible)
    |> Seq.sum

printfn "Part 1 checksum = %d" (part1())
printfn "Part 2 checksum = %d" (part2())