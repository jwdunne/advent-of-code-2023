open Base
open Stdio

let is_digit d = match d with '0' .. '9' -> true | _ -> false

let first_digit line =
  let rec aux chars =
    match chars with
    | 'o' :: 'n' :: 'e' :: _ ->
        1
    | 't' :: 'w' :: 'o' :: _ ->
        2
    | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ ->
        3
    | 'f' :: 'o' :: 'u' :: 'r' :: _ ->
        4
    | 'f' :: 'i' :: 'v' :: 'e' :: _ ->
        5
    | 's' :: 'i' :: 'x' :: _ ->
        6
    | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ ->
        7
    | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ ->
        8
    | 'n' :: 'i' :: 'n' :: 'e' :: _ ->
        9
    | c :: _ when is_digit c ->
        Option.value ~default:0 (Char.get_digit c)
    | _ :: rest ->
        aux rest
    | [] ->
        0
  in
  aux (String.to_list line)

let last_digit line =
  let rec aux chars =
    match chars with
    | 'e' :: 'n' :: 'o' :: _ ->
        1
    | 'o' :: 'w' :: 't' :: _ ->
        2
    | 'e' :: 'e' :: 'r' :: 'h' :: 't' :: _ ->
        3
    | 'r' :: 'u' :: 'o' :: 'f' :: _ ->
        4
    | 'e' :: 'v' :: 'i' :: 'f' :: _ ->
        5
    | 'x' :: 'i' :: 's' :: _ ->
        6
    | 'n' :: 'e' :: 'v' :: 'e' :: 's' :: _ ->
        7
    | 't' :: 'h' :: 'g' :: 'i' :: 'e' :: _ ->
        8
    | 'e' :: 'n' :: 'i' :: 'n' :: _ ->
        9
    | c :: _ when is_digit c ->
        Option.value ~default:0 (Char.get_digit c)
    | _ :: rest ->
        aux rest
    | [] ->
        0
  in
  aux (String.to_list (String.rev line))

let int_from_line line = (first_digit line * 10) + last_digit line

let part1 lines =
  lines
  |> List.map ~f:(fun line ->
         let rec first chars =
           match chars with
           | c :: _ when is_digit c ->
               Option.value ~default:0 (Char.get_digit c)
           | _ :: rest ->
               first rest
           | [] ->
               0
         in
         (first (String.to_list line) * 10)
         + first (String.to_list (String.rev line)) )
  |> List.fold ~f:( + ) ~init:0

let part2 lines =
  lines |> List.map ~f:int_from_line |> List.fold ~f:( + ) ~init:0

let () =
  let lines = In_channel.read_all "input" |> String.split_lines in
  printf "%d\n" @@ part1 lines ;
  printf "%d\n" @@ part2 lines
