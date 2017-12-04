open System.IO

let mutable index = 0
let mutable found = false

let result =
    File.ReadAllBytes "input.txt"
    |> Array.fold (fun floor b ->
        let nextPos =
            match char b with
            | '(' -> floor + 1
            | ')' -> floor - 1
            | _ -> failwith "Invalid character found"
        if not found && nextPos = -1 then
            found <- true
            printfn "First entered basement at %d" (index + 1)
        else
            index <- index + 1
        nextPos
    ) 0

printfn "Target floor is %d" result
