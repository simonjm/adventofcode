open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

type Claim = {
    ID: int
    Coords: (int * int) list
}

let regex = Regex "^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)"

let parseLine (line:string) = 
    let matches = regex.Match line
    if not matches.Success then failwithf "Regex failed on '%s'" line
    let xCoord = int(matches.Groups.[2].Value)
    let yCoord = int(matches.Groups.[3].Value)
    let width = int(matches.Groups.[4].Value)
    let height = int(matches.Groups.[5].Value)
    let coords = [
        for y = yCoord + 1 to yCoord + height do
            for x = xCoord + 1 to xCoord + width do
                yield (x, y)
    ]
    {
        ID = int(matches.Groups.[1].Value);
        Coords = coords;
    }

let createMap claims =
    let coords = Dictionary<(int * int), int>()
    for claim in claims do
        for point in claim.Coords do
            match coords.TryGetValue point with
            | (true, count) -> coords.[point] <- count + 1
            | (false, _) -> coords.[point] <- 1
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

let claims = 
    File.ReadLines "input.txt"
    |> Seq.map parseLine
    |> Seq.toList

let claimMap = createMap claims
// debugMap claimMap

let part1() =
    claimMap
    |> Seq.filter (fun kv -> kv.Value >= 2)
    |> Seq.length

let part2() =
    let noOverlap =
        claims
        |> Seq.find (fun claim -> 
            claim.Coords |> Seq.forall (fun point -> claimMap.[point] = 1)
        )
    noOverlap.ID

part1() |> printfn "Part 1 = %d"
part2() |> printfn "Part 2 = %d"


