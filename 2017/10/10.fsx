open System.IO

type State = {
    Current: int
    Skip: int
}

let values = [| for i in 0 .. 255 -> i |]
// let values = [| for i in 0 .. 4 -> i |]

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

let initialState = { Current = 0; Skip = 0 }

File.ReadAllText("input.txt").Split(',')
|> Seq.map int
// [3; 4; 1; 5]
|> Seq.fold folder initialState

printfn "Part 1 = %d" <| values.[0] * values.[1]