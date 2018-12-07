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

let findMatch (word1:string) (word2:string) =
    let getChars = Seq.mapi (fun i c -> (i, c))
    let chars1 = getChars word1
    let chars2 = getChars word2
    let results =
        Seq.zip chars1 chars2
        |> Seq.filter (fun ((_, c1), (_, c2)) -> c1 <> c2)
        |> Seq.map (fst >> fst)
        |> Seq.toList
    
    match results with
    | [i] -> Some(word1.Remove(i, 1))
    | _ -> None

let matchesSeq() = seq {
    let words = lines |> Seq.toArray
    for i = 0 to words.Length - 1 do
        for j = 0 to words.Length - 1 do
            if i = j then yield None
            else 
                yield findMatch words.[i] words.[j]
}

let part2() =
    matchesSeq() |> Seq.pick id

part1() |> printfn "Part 1 = %d"
part2() |> printfn "Part 2 = %s"
