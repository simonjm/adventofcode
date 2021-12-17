open System.IO;

type Op =
    | Forward of int
    | Down of int
    | Up of int

let lines = 
    File.ReadLines "input.txt" 
    |> Seq.map (fun str ->
        let parts = str.Split " "
        match parts with
        | [| "forward"; num |] -> Forward (int num)
        | [| "down"; num |] -> Down (int num)
        | [| "up"; num |] -> Up (int num)
        | _ -> failwithf "Unknown command '%s'" str
    )
    |> Seq.toList

let part1() =
    let (x, y) =
        lines
        |> Seq.fold (fun (x, y) cmd ->
            match cmd with
            | Forward num -> (x + num, y)
            | Down num -> (x, y + num)
            | Up num -> (x, y - num)
        ) (0, 0)
    x * y
    
let part2() =
    let (x, y, _) =
        lines
        |> Seq.fold (fun (x, y, aim) cmd ->
            match cmd with
            | Forward num -> (x + num, y + (aim * num), aim)
            | Down num -> (x, y, aim + num)
            | Up num -> (x, y, aim - num)
        ) (0, 0, 0)
    x * y
    

printfn "Part 1 = %d" <| part1()
printfn "Part 2 = %d" <| part2()
