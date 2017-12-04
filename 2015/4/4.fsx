open System
open System.Security.Cryptography
open System.Text

let input = "iwrupvqb"

let findHashWith f =
    let hash = MD5.Create()
    Seq.initInfinite (fun i -> i + 1)
    |> Seq.filter (fun i -> 
        let bytes = Encoding.ASCII.GetBytes(sprintf "%s%d" input i)
        let hashedBytes = hash.ComputeHash(bytes)
        f hashedBytes
    )
    |> Seq.head
    |> printfn "%d"


let with5Zeroes() = findHashWith (fun hashedBytes -> hashedBytes.[0] = 0uy && hashedBytes.[1] = 0uy && hashedBytes.[2] < 16uy)
let with6Zeroes() = findHashWith (fun hashedBytes -> hashedBytes.[0] = 0uy && hashedBytes.[1] = 0uy && hashedBytes.[2] = 0uy)

with5Zeroes()
