open System.IO
open System.Collections.Generic

type Op =
    | Inc of int
    | Dec of int

type Instruction = {
    DestRegister: string;
    Op: Op;
    Condition: unit -> bool
}

let registers = Dictionary<string, int>()

let parseLine (parts:string[]) =
    let r = parts.[0]
    if not <| registers.ContainsKey r then registers.[r] <- 0

    let num = int parts.[2]
    let op =
        match parts.[1] with
        | "inc" -> Inc num
        | "dec" -> Dec num
        | o -> failwithf "Invalid operation %s" o
    let cond() =
        let left = registers.[parts.[4]]
        let right = int <| parts.[6]
        match parts.[5] with
        | ">" -> left > right
        | "<" -> left < right
        | ">=" -> left >= right
        | "<=" -> left <= right
        | "!=" -> left <> right
        | "==" -> left = right
        | o -> failwithf "Invalid operand %s" o
    { DestRegister = r; Op = op; Condition = cond }

let instructions =
    File.ReadLines "input.txt"
    |> Seq.map ((fun l -> l.Split ' ') >> parseLine)
    |> Seq.toList

for instr in instructions do
    if instr.Condition() then
        let v = registers.[instr.DestRegister]
        let result = 
            match instr.Op with 
            | Inc x -> v + x 
            | Dec x -> v - x
        registers.[instr.DestRegister] <- result

let max = registers.Values |> Seq.max
printfn "Part 1 = %d" max

