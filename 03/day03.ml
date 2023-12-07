open Base
open Stdio

let compare_int_pair (x0, y0) (x1, y1) =
  match Int.compare x0 x1 with 0 -> Int.compare y0 y1 | c -> c

module Coord = struct
  module T = struct
    type t = int * int

    let compare = compare_int_pair

    let sexp_of_t (x, y) = Sexp.List [Int.sexp_of_t x; Int.sexp_of_t y]
  end

  include T
  include Comparable.Make (T)
end

let empty_coord_set = Set.empty (module Coord)

module SymbolKey = struct
  type t = char

  let compare = compare_char
end

module SymbolMap = Stdlib.Map.Make (SymbolKey)
module CoordMap = Stdlib.Map.Make (Coord)

let is_digit c = match c with '0' .. '9' -> true | _ -> false

let all_digits = List.for_all ~f:is_digit

let parse_numbers lines =
  let add map coord digits = CoordMap.add coord digits map in
  let rec parse_line i chars j numbers =
    match chars with
    | a :: b :: c :: rest when all_digits [a; b; c] ->
        parse_line i rest (j + 3) (add numbers (j, i) [a; b; c])
    | a :: b :: rest when all_digits [a; b] ->
        parse_line i rest (j + 2) (add numbers (j, i) [a; b])
    | a :: rest when is_digit a ->
        parse_line i rest (j + 1) (add numbers (j, i) [a])
    | _ :: rest ->
        parse_line i rest (j + 1) numbers
    | [] ->
        numbers
  in
  let coords_to_digits =
    List.foldi lines ~init:CoordMap.empty ~f:(fun i map line ->
        parse_line i (String.to_list line) 0 map )
  in
  let coords_to_root =
    CoordMap.fold
      (fun (x, y) digits map ->
        List.foldi ~init:map
          ~f:(fun i map _ -> CoordMap.add (x + i, y) (x, y) map)
          digits )
      coords_to_digits CoordMap.empty
  in
  (coords_to_digits, coords_to_root)

let parse_symbols lines =
  let add_or_update map key coord =
    let set =
      map |> SymbolMap.find_opt key |> Option.value ~default:empty_coord_set
    in
    SymbolMap.add key (Set.add set coord) map
  in
  let rec parse_line i chars j symbols =
    match chars with
    | ('.' | '0' .. '9') :: rest ->
        parse_line i rest (j + 1) symbols
    | c :: rest ->
        parse_line i rest (j + 1) (add_or_update symbols c (j, i))
    | [] ->
        symbols
  in
  List.foldi lines ~init:SymbolMap.empty ~f:(fun i map line ->
      parse_line i (String.to_list line) 0 map )

let adjacent_points (x, y) =
  [ (x - 1, y - 1)
  ; (x, y - 1)
  ; (x + 1, y - 1)
  ; (x + 1, y)
  ; (x + 1, y + 1)
  ; (x, y + 1)
  ; (x - 1, y + 1)
  ; (x - 1, y) ]

let part1 lines =
  let coords_to_numbers, coords_to_roots = parse_numbers lines in
  let symbols =
    SymbolMap.fold
      (fun _ set coords -> Set.union set coords)
      (parse_symbols lines) empty_coord_set
  in
  symbols |> Set.to_list
  |> List.concat_map ~f:adjacent_points
  |> List.filter_map ~f:(fun coord -> CoordMap.find_opt coord coords_to_roots)
  |> List.dedup_and_sort ~compare:compare_int_pair
  |> List.filter_map ~f:(fun coord -> CoordMap.find_opt coord coords_to_numbers)
  |> List.map ~f:(fun x -> x |> String.of_list |> Int.of_string)
  |> List.fold ~f:( + ) ~init:0

let part2 lines =
  let coords_to_numbers, coords_to_roots = parse_numbers lines in
  let gears = lines |> parse_symbols |> SymbolMap.find '*' |> Set.to_list in
  gears
  |> List.map ~f:(fun coords ->
         coords |> adjacent_points
         |> List.filter_map ~f:(fun coord ->
                CoordMap.find_opt coord coords_to_roots )
         |> List.dedup_and_sort ~compare:compare_int_pair )
  |> List.filter ~f:(fun coords -> List.length coords >= 2)
  |> List.map ~f:(fun coords ->
         List.map ~f:(fun coord -> CoordMap.find coord coords_to_numbers) coords )
  |> List.map ~f:(fun nums ->
         List.map ~f:(fun num -> num |> String.of_list |> Int.of_string) nums )
  |> List.map ~f:(fun nums -> List.fold ~f:( * ) ~init:1 nums)
  |> List.fold ~f:( + ) ~init:0

let () =
  let lines = In_channel.read_all "input" |> String.split_lines in
  printf "%d\n" @@ part1 lines ;
  printf "%d\n" @@ part2 lines
