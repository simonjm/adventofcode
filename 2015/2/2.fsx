open System
open System.IO

type Dimension(l: int, w: int, h: int) =
    let volume = l * w * h
    member this.GetTotalRibbon() =
        Math.Min(2 * (l + w), Math.Min(2 * (h + w), 2 * (l + h))) + volume
    member this.GetTotalPaper() =
        let area1 = l * w
        let area2 = w * h
        let area3 = h * l
        (2 * area1) + (2 * area2) + (2 * area3) + (Math.Min(area1, Math.Min(area2, area3)))

let presents =
    File.ReadLines "input.txt"
    |> Seq.map (fun line ->
        let parts = line.Split 'x'
        new Dimension(int(parts.[0]), int(parts.[1]), int(parts.[2]))
    )
    |> Seq.toList

let totalPaper = presents |> Seq.map (fun d -> d.GetTotalPaper()) |> Seq.sum
let totalRibbon = presents |> Seq.map (fun d -> d.GetTotalRibbon()) |> Seq.sum

printfn "Total wrapping paper needed is %d feet and total ribbon is %d" totalPaper totalRibbon
