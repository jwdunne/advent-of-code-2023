open Stdio
open Base

let rec diffs nums =
  let rec aux differences = function
    | [] | _ :: [] -> differences
    | a :: b :: t -> aux (b - a :: differences) (b :: t) in
  List.rev (aux [] nums)

let hist nums =
  let rec aux h nums =
    let xs = diffs nums in
    if List.for_all ~f:(equal 0) xs then (nums :: h)
    else aux (nums :: h) xs in
  aux [] nums

let next nums =
  hist nums |> List.map ~f:List.last_exn |> List.fold ~f:(+) ~init:0 

let prev nums =
  let rec aux n = function
    | [] -> n
    | a :: t -> aux ((List.hd_exn a) - n) t in
  aux 0 (hist nums)

let part1 nums = nums |> List.map ~f:next |> List.fold ~f:(+) ~init:0

let part2 nums = nums |> List.map ~f:prev |> List.fold ~f:(+) ~init:0

let input filename =
  let parse_line line =
    line |> String.split ~on:' ' |> List.map ~f:Int.of_string in
  In_channel.read_all filename |> String.split_lines |> List.map ~f:parse_line

let () = 
  let nums = input "input" in
  printf "%d\n" @@ part1 nums;
  printf "%d\n" @@ part2 nums
