open System

let groupByNum (num:string) =
    let rec loop prev (acc:(char * int) list) left =
        match left with
        | [] -> acc
        | h :: t when h = prev -> 
            let (c, total) = acc.Head
            loop h ((c, total + 1) :: acc.Tail) t
        | h :: t -> 
            loop h ((h, 1) :: acc) t
    let left = num.ToCharArray() |> Seq.toList 
    let first = left.Head
    loop first [(first, 1)] left.Tail |> List.rev

let lookAndSay (num:string) =
    let nums =
        groupByNum num
        |> Seq.collect (fun (c, numChars) -> 
            [sprintf "%d" numChars; c.ToString()]
        )
    String.Concat(nums)

let iterations = 50
let input = "1113122113"
let run n = 
    seq { 1..n } 
    |> Seq.fold (fun acc _ -> 
        // printfn "Iteration %d = %s" n acc
        lookAndSay acc
    ) input

let part1() = run 40
let part2() = run 50

printfn "%d" (part1()).Length
