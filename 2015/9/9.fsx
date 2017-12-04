open System
open System.IO
open System.Text.RegularExpressions

type City = {
    Name: string;
}

type Route = {
    Start: City;
    End: City;
    Distance: int;
}

let expr = new Regex @"(\w+) to (\w+) = (\d+)"
let parseLine (line:string) =
    let matches = expr.Match line    
    {Start = {Name = matches.Groups.[1].Value}; End = {Name = matches.Groups.[2].Value}; Distance = int matches.Groups.[3].Value}

File.ReadLines "input.txt"
|> Seq.map parseLine
