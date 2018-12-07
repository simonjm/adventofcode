open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

type Claim = {
    ID: int
    XCoord: int
    YCoord: int
    Width: int
    Height: int
}

let regex = Regex "^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)"

let parseLine (line:string) = 
    let matches = regex.Match line
    if not matches.Success then failwithf "Regex failed on '%s'" line
    {
        ID = int(matches.Groups.[1].Value);
        XCoord = int(matches.Groups.[2].Value);
        YCoord = int(matches.Groups.[3].Value);
        Width = int(matches.Groups.[4].Value);
        Height = int(matches.Groups.[5].Value);
    }

let createMap claims =
    let coords = Dictionary<(int * int), int>()
    for claim in claims do
        for y = claim.YCoord + 1 to claim.YCoord + claim.Height do
            for x = claim.XCoord + 1 to claim.XCoord + claim.Width do
                let point = (x, y)
                let found, count = coords.TryGetValue point
                if found then coords.[point] <- count + 1
                else coords.[point] <- 1
    coords

let debugMap (coords:Dictionary<int *int, int>) = 
    use stream = File.OpenWrite("debug.txt")
    use writer = new StreamWriter(stream)
    for y = 0 to 2000 do
        let row = List<string>()
        for x = 0 to 3000 do
            let point = (x, y)
            let found, count = coords.TryGetValue point
            if found then row.Add(count.ToString())
            else row.Add "."
        writer.WriteLine(String.concat " " row)

let part1() =
    let map =
        File.ReadLines "input.txt"
        |> Seq.map parseLine
        |> createMap
    // debugMap map
    map
    |> Seq.filter (fun kv -> kv.Value >= 2)
    |> Seq.length

part1() |> printfn "Part 1 = %d"


