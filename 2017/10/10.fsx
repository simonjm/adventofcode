open System
open System.IO
open System.Text

type State = {
    Current: int
    Skip: int
}

let hash (values:int[]) (lengths:seq<int>) rounds = 
    let folder (prev:State) length =
        let start = prev.Current 
        let last = start + length

        // reverse the sublist
        let newList = [
            for i = last - 1 downto start do
                yield values.[i % values.Length]
        ]

        // apply the results to the array
        newList 
        |> List.fold (fun i v -> 
            values.[i % values.Length] <- v
            i + 1
        ) start
        |> ignore
        { Current = (last + prev.Skip) % values.Length; Skip = prev.Skip + 1 }

    let mutable state = { Current = 0; Skip = 0 }
    for i = 1 to rounds do
        state <- Seq.fold folder state lengths

let values = [ for i in 0 .. 255 -> i ]
let input = File.ReadAllText("input.txt")

let toInts (strings:string) = 
    strings.Split([|','|], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map int

let reduceHash (hash:int[]) = 
    hash
    |> Seq.chunkBySize 16
    |> Seq.map (Array.reduce (^^^))
    |> Seq.map (fun i -> i.ToString("X").PadLeft(2, '0'))
    |> String.Concat

let part1 (value:string) =
    let valuesCopy = List.toArray values
    let lengths = toInts value
    hash valuesCopy lengths 1
    valuesCopy.[0] * valuesCopy.[1]

let part2 (value:string) =
    let suffix = "17,31,73,47,23"
    let bytes = Encoding.ASCII.GetBytes value
    let ascii = String.Join(",", bytes)
    let lengths = 
        String.Join(",", ascii, suffix)
        |> toInts
        |> Seq.toList

    let valuesCopy = List.toArray values
    hash valuesCopy lengths 64
    reduceHash valuesCopy

printfn "Part 1 = %d" <| part1 input
printfn "Part 2 = %s" <| part2 input