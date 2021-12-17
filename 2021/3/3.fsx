open System;
open System.IO;

let lines = 
    File.ReadLines "input.txt" 
    |> Seq.map (fun str -> Convert.ToInt32(str, 2))
    |> Seq.toList

let part1() =
    let mutable gamma = 0
    let mutable epsilon = 0
    for i = 11 downto 0 do
        let mask = int(2.0 ** double(i))
        let (zero, one) =
            lines
            |> Seq.fold (fun (zero, one) num -> 
                /// use bitwise AND to check if a bit is set in the i position
                let result = num &&& mask
                match result with
                | 0 -> (zero + 1, one)
                | v when v = int(mask) -> (zero, one + 1)
                | _ -> (zero, one)
            ) (0, 0)
        if one > zero then gamma <- gamma ^^^ mask // one's were the most common bit so flip the position in gamma
        else epsilon <- epsilon ^^^ mask // one's were the least common bit so flip in epsilon
    gamma * epsilon
    

printfn "Part 1 = %d" <| part1()
// printfn "Part 2 = %d" <| part2()
