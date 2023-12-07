open Stdio
open Base

type range = int * int

type mapping = {src: range; dest: range}

let space_r = Str.regexp " "

let seed_r = Str.regexp {|seeds: \(.*\)$|}

let parse_seeds block =
  let _ = Str.string_match seed_r block 0 in
  Str.matched_group 1 block |> Str.split space_r |> List.map ~f:Int.of_string
  |> Array.of_list

let parse_mapping line =
  let parts = line |> Str.split space_r |> List.map ~f:Int.of_string in
  let dst = List.nth_exn parts 0 in
  let src = List.nth_exn parts 1 in
  let len = List.nth_exn parts 2 in
  {src= (src, src + len); dest= (dst, dst + len)}

let parse_map block =
  String.split_lines block |> List.tl_exn |> List.map ~f:parse_mapping
  |> Array.of_list

let parse_blocks str =
  let blocks = Str.split (Str.regexp "\n\n") str in
  let maps = blocks |> List.tl_exn |> List.map ~f:parse_map |> Array.of_list in
  (parse_seeds (List.hd_exn blocks), maps)

let _in n range = n >= fst range && n < snd range

let next seed map =
  let in_range seed mapping = _in seed mapping.src in
  match Array.find ~f:(in_range seed) map with
  | Some mapping ->
      seed + (fst mapping.dest - fst mapping.src)
  | None ->
      seed

let follow maps seed = Array.fold ~f:next ~init:seed maps

let lowest_destination seeds maps =
  seeds
  |> Array.map ~f:(follow maps)
  |> Array.min_elt ~compare:compare_int
  |> Option.value_exn

let invert map =
  let invert_mapping m = {src= m.dest; dest= m.src} in
  map |> Array.rev |> Array.map ~f:(Array.map ~f:invert_mapping)

let part1 str =
  let seeds, maps = parse_blocks str in
  lowest_destination seeds maps

let part2 str =
  let seed_ranges, maps = parse_blocks str in
  let inverted = invert maps in
  let seeds =
    seed_ranges |> List.of_array |> List.chunks_of ~length:2
    |> List.map ~f:(fun range ->
           (List.hd_exn range, List.hd_exn range + List.last_exn range) )
    |> Array.of_list
  in
  let rec find_seed n =
    if
      Array.exists seeds ~f:(fun range ->
          let seed = follow inverted n in
          _in seed range && phys_equal (follow maps seed) n )
    then n
    else find_seed (n + 1)
  in
  find_seed 0

let () =
  let str = In_channel.read_all "input" in
  printf "%d\n" @@ part1 str;
  printf "%d\n" @@ part2 str
