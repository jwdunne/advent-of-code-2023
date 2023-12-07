open Stdio
open Base

type race = {time: int; distance: int}

let header_r = Str.regexp {|[A-Z][a-z]+: *\(.*\)|}

let parse_line line =
  let _ = Str.string_match header_r line 0 in
  Str.matched_group 1 line
  |> Str.split (Str.regexp " +")
  |> List.map ~f:Int.of_string

let parse_races str =
  let lines = String.split_lines str in
  let times = parse_line (List.hd_exn lines) in
  let distances = parse_line (List.last_exn lines) in
  List.zip_exn times distances
  |> List.map ~f:(fun (time, distance) -> {time; distance})

let ways_to_win race =
  let rec min_to_win s =
    if (race.time - s) * s > race.distance then s else min_to_win (s + 1)
  in
  let rec max_to_win s =
    if (race.time - s) * s > race.distance then s else max_to_win (s - 1)
  in
  max_to_win race.time - min_to_win 0 + 1

let part1 races = races |> List.map ~f:ways_to_win |> List.fold ~f:( * ) ~init:1

let part2 races =
  let time =
    races
    |> List.map ~f:(fun x -> Int.to_string x.time)
    |> String.concat |> Int.of_string
  in
  let distance =
    races
    |> List.map ~f:(fun x -> Int.to_string x.distance)
    |> String.concat |> Int.of_string
  in
  ways_to_win {time; distance}

let () =
  let races = In_channel.read_all "input" |> parse_races in
  printf "%d\n" @@ part1 races;
  printf "%d\n" @@ part2 races
