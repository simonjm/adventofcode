open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

type EventType =
    | StartShift of int
    | Sleeping
    | Awake

let regex = Regex "^\[(.+?)\] (.+)"
let guardRegex = Regex "^Guard #(\d+)"

let parseEvent (eventType:string) = 
    match eventType.TrimEnd() with
    | "falls asleep" -> Sleeping
    | "wakes up" -> Awake
    | ev ->
        let matches = guardRegex.Match ev
        if not matches.Success then failwithf "Regex failed on '%s'" ev
        let id = int(matches.Groups.[1].Value)
        StartShift id

let parseLine (line:string) = 
    let matches = regex.Match line
    if not matches.Success then failwithf "Regex failed on '%s'" line
    let date = DateTime.ParseExact(matches.Groups.[1].Value, "yyyy-MM-dd HH:mm", null)
    let eventType = parseEvent matches.Groups.[2].Value
    (date, eventType)

let chunk (events:seq<DateTime * EventType>) =
    let range = TimeSpan.FromHours 2
    let rec loop acc cur time left = 
        match left with
        | [] -> acc
        | (date, _) :: t -> ()
    let first = List.head events |> fst
    loop [] [] first (List.tail events)

let sortedEvents = 
    File.ReadLines "input.txt"
    |> Seq.map parseLine
    |> Seq.sortBy fst
    |> chunk
    |> Seq.toList

let part1() = 
    let dict = Dictionary<int, int>()
    sortedEvents 
    |> Seq.fold (fun (onDutyGuard, sleepTime) (timestamp, eventType) ->
        match eventType with
        | StartShift id -> (id, timestamp)
        | Sleeping -> (onDutyGuard, timestamp)
        | Awake -> 
            let minutesSlept = int(timestamp.Subtract(sleepTime).TotalMinutes)
            match dict.TryGetValue onDutyGuard with
            | (true, total) -> dict.[onDutyGuard] <- total + minutesSlept
            | (false, _) -> dict.[onDutyGuard] <- minutesSlept
            (onDutyGuard, timestamp)
    ) (-1, DateTime.MinValue)
    |> ignore
    let mostSlept =
        dict |> Seq.maxBy (fun kv -> kv.Value)
    mostSlept.Key * mostSlept.Value

part1() |> printfn "Part 1 = %d"
    