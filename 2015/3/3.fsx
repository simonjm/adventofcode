open System.IO
open System.Collections.Generic

let houseMap = new Dictionary<int * int, int>()
let sumHouseMap() =
    houseMap.Keys
    |> Seq.choose (fun key ->
        let found, count = houseMap.TryGetValue key
        if found && count >= 1 then Some 1 else None
    )
    |> Seq.sum

type DelivererCoords = {
    Santa: int * int;
    Robo: int * int;
}

type DelivererTurn =
    | Santa
    | Robo
    | Both

let addToHouseMap coords =
    match houseMap.TryGetValue coords with
    | (true, count) -> houseMap.[coords] <- count + 1
    | _ -> houseMap.Add (coords, 1) 

let parseDirection direction coords =
    let x, y = coords
    match char direction with
    | '^' -> (x, y + 1)
    | 'v' -> (x, y - 1)
    | '>' -> (x + 1, y)
    | '<' -> (x - 1, y)
    | _ -> failwith "Invalid char found"

File.ReadAllBytes "input.txt"
|> Array.fold (fun coordsAndDeliverer direction ->
    let locations, turn = coordsAndDeliverer
    match turn with
    | Santa ->
        let coords = locations.Santa
        addToHouseMap coords
        ({Santa = (parseDirection direction coords); Robo = locations.Robo}, Robo)
    | Robo ->
        let coords = locations.Robo
        addToHouseMap coords
        ({Robo = (parseDirection direction coords); Santa = locations.Santa}, Santa)
    | Both -> 
        addToHouseMap locations.Santa
        addToHouseMap locations.Robo
        ({Santa = (parseDirection direction locations.Santa); Robo = locations.Robo}, Robo)
) ({Santa = (0, 0); Robo = (0, 0)}, Both)

printfn "Total with only 1 present. %d" (sumHouseMap())
