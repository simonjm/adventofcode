open System.IO

type State = {
    InGarbage: bool;
    IgnoreMode: bool;
    Score: int;
    OpenGroup: int;
    GarbageCount: int
}

let folder (prev:State) cur =
    match cur with
    | _ when prev.IgnoreMode -> { prev with IgnoreMode = false }
    | '<' when not prev.InGarbage -> { prev with InGarbage = true }
    | '!' when not prev.IgnoreMode -> { prev with IgnoreMode = true }
    | '!' -> prev
    | '>' when prev.InGarbage -> { prev with InGarbage = false }
    | _ when prev.InGarbage -> { prev with GarbageCount = prev.GarbageCount + 1 }
    | '{' -> { prev with OpenGroup = prev.OpenGroup + 1 }
    | '}' when prev.OpenGroup > 0 -> { prev with OpenGroup = prev.OpenGroup - 1; Score = prev.Score + prev.OpenGroup }
    | _ -> prev

let initialState = { InGarbage = false; IgnoreMode = false; Score = 0; OpenGroup = 0; GarbageCount = 0 }
let state =
    File.ReadAllText "input.txt"
    |> Seq.fold folder initialState

printfn "Part 1 = %d" state.Score
printfn "Part 2 = %d" state.GarbageCount