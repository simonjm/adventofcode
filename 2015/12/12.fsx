#r "../../packages/Newtonsoft.Json.6.0.8/lib/net45/Newtonsoft.Json.dll"
open System
open System.IO
open Newtonsoft.Json

type SumType =
    | Valid of sum: int64
    | Invalid

let getSum s =
    match s with
    | Valid sum -> sum
    | Invalid -> 0L

let getNumbers (reader:JsonTextReader) = seq {
    while reader.Read() do
        match reader.TokenType with
        | JsonToken.Integer -> yield reader.Value :?> Int64
        | _ -> ()
}

let sumObj (reader:JsonTextReader) = 
    let rec loop (sum:SumType) =
        if not (reader.Read()) then Valid 0L
        else
            match reader.TokenType with
            | JsonToken.EndObject -> sum
            | JsonToken.String when (reader.Value :?> string) = "red" -> Invalid
            | JsonToken.StartObject -> 
                match loop (Valid 0L) with
                | Valid innerSum ->
                    let s = Valid(getSum sum + innerSum)
                    loop s
                | Invalid -> Valid 0L
            | JsonToken.Integer -> 
                loop (Valid(getSum sum + (reader.Value :?> int64)))
            | _ -> loop sum
    loop (Valid 0L)

let getNumbers2 (reader:JsonTextReader) = seq {
    while reader.Read() do
        match reader.TokenType with
        | JsonToken.Integer -> yield reader.Value :?> Int64
        | JsonToken.StartObject -> yield getSum(sumObj reader)
        | _ -> ()
}

let reader = File.OpenText "input.txt"
let jsonReader = new JsonTextReader(reader)

let result = getNumbers2 jsonReader |> Seq.sum
printfn "%d" result