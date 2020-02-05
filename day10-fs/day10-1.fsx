open System.IO

type Point = int * int
type Area = {
  Data: Map<Point, unit>
  Width: int
  Height: int
}

module Seq =
  let foldi folder seed =
    Seq.mapi (fun i x -> i, x) >> Seq.fold (fun acc (i, x) -> folder acc i x) seed

let inline square x = x * x
let inline fdiv a b = double a / double b

module Point =
  let distance x1 y1 x2 y2 = (square (x2 - x1) + square (y2 - y1)) |> double |> sqrt

  // Draw a line segment S1 from (x1,y1) to (x2, y2), draw S2 from (x1, y1) to (x3, y3).
  // Return true if S2 contains S1.
  let segmentOverlaps (x1, y1) (x2, y2) (x3, y3) =
    if y1 = y2 then // horizontal line
      if y3 <> y2 then false
      elif x2 > x1 then x3 > x2 // []--X---X points right
      else x3 < x2 // X--X-[] points left
    else
      let h1, h2, v1, v2 = x2 - x1, x3 - x1, y2 - y1, y3 - y1
      // vector should has the same direction and angle
      sign h1 = sign h2 && fdiv h1 v1 = fdiv h2 v2

module Area =
  let parse input =
    let rec parseLine (y: int) =
      Seq.foldi (fun map x c -> if c = '#' then Map.add (x, y) () map else map)

    let data = input |> Seq.foldi (fun map y line -> parseLine y map line) Map.empty
    {Data = data; Width = String.length (Array.get input 0); Height = input.Length}

  let getVisible (x, y) area =
    // Given a queue of other asteroids sorted by distance,
    // remove all obstructed by the first item in the queue
    let rec loop acc = function
    | [] -> acc
    | p1::tl ->
      let notHidden = List.filter (fun p2 -> not <| Point.segmentOverlaps (x, y) p1 p2) tl
      loop (p1::acc) notHidden

    area.Data
    |> Map.toSeq
    |> Seq.map fst
    |> Seq.except [x, y]
    |> Seq.sortBy (fun (x2, y2) -> Point.distance x y x2 y2)
    |> List.ofSeq
    |> loop []

  let toVisibility area =
    area.Data |> Map.map (fun p _ -> getVisible p area)

let inputFileName = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
let area = inputFileName |> File.ReadAllLines |> Area.parse

let visibility = Area.toVisibility area
// 329
let part1 = visibility |> Map.map (fun _ ls -> List.length ls + 1) |> Map.toSeq |> Seq.maxBy snd
