open Stdio
open Base

type int_set = (int, Int.comparator_witness) Set.t

type card = {n: int; numbers: int_set; winners: int_set}

let parse_numbers numbers =
  numbers
  |> Str.split (Str.regexp " +")
  |> List.map ~f:Int.of_string
  |> Set.of_list (module Int)

let card_r = Str.regexp {|Card +\([0-9]+\): \(.*\) | \(.*\)|}

let parse_card line =
  let _ = Str.string_match card_r line 0 in
  let n = line |> Str.matched_group 1 |> Int.of_string in
  let winners = line |> Str.matched_group 2 in
  let numbers = line |> Str.matched_group 3 in
  {n; numbers= parse_numbers numbers; winners= parse_numbers winners}

let parse_cards lines = List.map ~f:parse_card lines

let winners card = Set.inter card.numbers card.winners

let part1 lines =
  lines |> parse_cards |> List.map ~f:winners
  |> List.filter ~f:(fun s -> s |> Set.is_empty |> not)
  |> List.map ~f:(fun xs -> Int.pow 2 (Set.length xs - 1))
  |> List.fold ~f:( + ) ~init:0

let part2 lines =
  let cards = parse_cards lines |> List.to_array in
  let select_winners card =
    let count = card |> winners |> Set.length in
    List.range card.n (card.n + count)
    |> List.map ~f:(Array.get cards)
    |> List.to_array
  in
  let rec aux count checks =
    if Array.is_empty checks then count
    else
      let wins = Array.concat_map checks ~f:select_winners in
      aux (count + Array.length checks) wins
  in
  aux 0 cards

let () =
  let lines = In_channel.read_all "input" |> String.split_lines in
  printf "%d\n" @@ part1 lines ;
  printf "%d\n" @@ part2 lines
