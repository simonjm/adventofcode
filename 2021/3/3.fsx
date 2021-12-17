open System;
open System.IO;

let lines = 
    File.ReadLines "input.txt" 
    |> Seq.map (fun str -> Convert.ToInt32(str, 2))
    |> Seq.toList

let totalBits mask =
    lines
    |> Seq.fold (fun (zero, one) num -> 
        /// use bitwise AND to check if a bit is set in the i position
        let result = num &&& mask
        match result with
        | 0 -> (zero + 1, one)
        | v when v = int(mask) -> (zero, one + 1)
        | _ -> (zero, one)
    ) (0, 0)

let part1() =
    let mutable gamma = 0
    let mutable epsilon = 0
    for i = 11 downto 0 do
        let mask = int(2.0 ** double(i))
        let (zero, one) = totalBits mask
        if one > zero then gamma <- gamma ^^^ mask // one's were the most common bit so flip the position in gamma
        else epsilon <- epsilon ^^^ mask // one's were the least common bit so flip in epsilon
    gamma * epsilon
    
let part2() =
    let o2Rating (group: Map<int, seq<int>>) =
        let zeroCount = group.[0] |> Seq.length
        let oneCount = group.[1] |> Seq.length
        if zeroCount > oneCount then group.[0] |> Seq.toList
        else group.[1] |> Seq.toList

    let co2Rating (group: Map<int, seq<int>>) =
        let zeroCount = group.[0] |> Seq.length
        let oneCount = group.[1] |> Seq.length
        if zeroCount <= oneCount then group.[0] |> Seq.toList
        else group.[1] |> Seq.toList

    let rec loop i acc (criteria: Map<int, seq<int>> -> list<int>) = 
        match acc with
        | [v] -> v
        | _ ->
            let mask = int(2.0 ** double(i))
            let groups = 
                acc 
                |> Seq.groupBy (fun n ->
                    let result = n &&& mask
                    match result with
                    | 0 -> 0
                    | v when v = int(mask) -> 1
                    | _ -> failwithf "Failed to group number %d" result
                )
                |> Map.ofSeq
            
            let result = criteria groups
            loop (i - 1) result criteria

    let o2 = loop 11 lines o2Rating
    let co2 = loop 11 lines co2Rating
    o2 * co2

printfn "Part 1 = %d" <| part1()
printfn "Part 2 = %d" <| part2()
