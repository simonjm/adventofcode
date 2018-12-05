open System.IO;
open System.Collections.Generic;

type IDType =
    | TwoLetters
    | ThreeLetters
    | Both
    | Neither

let getLetterCounts (id:string) =
    let map = Dictionary<char, int>()
    for letter in id do
        if map.ContainsKey(letter) then
            let value = map.[letter]
            map.[letter] <- value + 1
        else
            map.[letter] <- 1
    map

let calculateType (counts:Dictionary<char, int>) = 
    let twoCount = counts |> Seq.filter (fun kv -> kv.Value = 2) |> Seq.length
    let threeCount = counts |> Seq.filter (fun kv -> kv.Value = 3) |> Seq.length
    if twoCount >= 1 && threeCount >= 1 then Both
    elif twoCount >= 1 then TwoLetters
    elif threeCount >= 1 then ThreeLetters
    else Neither

let lines = File.ReadLines "input.txt" |> Seq.toList

let part1() = 
    let (three, two) =
        lines
        |> Seq.map (getLetterCounts >> calculateType)
        |> Seq.fold (fun (three, two) cur -> 
            match cur with 
            | Both -> (three + 1, two + 1)
            | ThreeLetters -> (three + 1, two)
            | TwoLetters -> (three, two + 1)
            | Neither -> (three, two)
        ) (0, 0)
    three * two 

part1() |> printfn "Part 1 = %d"
