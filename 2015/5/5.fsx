open System
open System.IO

let part1 lines =
    let contains3Vowels (str:string) =
        str.ToCharArray()
        |> Array.sumBy (fun letter ->
            match letter with
            | 'a' | 'e' | 'i' | 'o' | 'u' -> 1
            | _ -> 0
        ) >= 3

    let twiceInARow (str:string) =
        let letters = str.ToCharArray() |> Array.toList
        let rec loop curLetter lettersLeft =
            match lettersLeft with
            | [] -> false
            | h :: t when curLetter = h -> true
            | h :: t -> loop h t
        loop letters.Head letters.Tail

    let containsInvalid (str:string) =
        str.Contains "ab" || str.Contains "cd" || str.Contains "pq" || str.Contains "xy"

    lines
    |> List.filter (fun line -> not (containsInvalid line) && contains3Vowels line && twiceInARow line)
    |> List.length

let part2 lines =
    let generatePairs (str:string) =
        Seq.windowed 2 str 
        |> Seq.map String.Concat
        |> Seq.toList

    let hasOverlappingPairs (pairs:string list) =
        let rec loop currentPair pairsLeft =
            match pairsLeft with
            | [] -> false
            // "aaa" has overlapping pairs but "aaaa" does not
            | h :: t when not t.IsEmpty && currentPair = h && h <> t.Head -> true
            | h :: t -> loop h t
        loop pairs.Head pairs.Tail

    let contains2Pairs (pairs:string list) =
        not (hasOverlappingPairs pairs) && pairs |> List.countBy id |> List.exists (fun (_, count) -> count > 1)

    let reverseStr (str:string) =
        str.ToCharArray() |> Array.rev |> String.Concat

    let has1LetterBetween (pairs:string list) =
        let rec loop currentPair pairsLeft =
            match pairsLeft with
            | [] -> false
            | h :: t when currentPair = (reverseStr h) -> true
            | h :: t -> loop h t
        loop pairs.Head pairs.Tail

    lines
    |> List.map (fun word -> (word, generatePairs word))
    |> List.filter (fun (word, pairs) -> contains2Pairs pairs && has1LetterBetween pairs)
    |> List.length

let lines = File.ReadAllLines "input.txt" |> List.ofArray
printfn "Part1 has %d nice strings" (part1 lines)
printfn "Part2 has %d nice strings" (part2 lines)
