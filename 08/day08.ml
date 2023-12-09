open Stdio
open Base

module Dir = struct
  type t = L | R

  let of_char = function 'L' -> Some L | 'R' -> Some R | _ -> None
end

module DirRing = struct
  type t = Dir.t list

  let of_string s = s |> String.to_list |> List.filter_map ~f:Dir.of_char
end

module Node = struct
  type t = string * string

  let node_r =
    Str.regexp {|(\([A-Z0-9][A-Z0-9][A-Z0-9]\), \([A-Z0-9][A-Z0-9][A-Z0-9]\))|}

  let of_string s =
    if not (Str.string_match node_r s 0) then None
    else Some (Str.matched_group 1 s, Str.matched_group 2 s)
end

module NodeMap = struct
  type t = (string, Node.t, String.comparator_witness) Map.t

  let empty = Map.empty (module String)

  let of_string s =
    let parse_node s =
      let open Option.Let_syntax in
      let parts = Str.split (Str.regexp " = ") s in
      let%bind key = List.hd parts in
      let%bind node_str = List.last parts in
      let%bind node = Node.of_string node_str in
      Some (key, node)
    in
    s |> String.split_lines
    |> List.filter_map ~f:parse_node
    |> List.fold
         ~f:(fun map (key, node) -> Map.set map ~key ~data:node)
         ~init:empty
end

let parse_map str =
  let open Option.Let_syntax in
  let parts = Str.split (Str.regexp "\n\n") str in
  let%bind dir_str = List.hd parts in
  let%bind node_map_str = List.last parts in
  Some (DirRing.of_string dir_str, NodeMap.of_string node_map_str)

let rec gcd a b = if b = 0 then a else gcd b (a % b)

let lcm a b = a * b / gcd a b

let part1 (dirs, nodes) =
  let open Option.Let_syntax in
  let%bind start = List.hd (Map.keys nodes) in
  let follow i (_, k) d =
    match (Map.find nodes k, d) with
    | None, _ ->
        (i + 1, k)
    | Some n, Dir.L ->
        (i + 1, fst n)
    | Some n, Dir.R ->
        (i + 1, snd n)
  in
  let rec aux start n =
    let i, dest = List.foldi ~f:follow ~init:(0, start) dirs in
    if not (String.equal dest "ZZZ") then aux dest (n + i) else n + i
  in
  Some (aux start 0)

let part2 (dirs, nodes) =
  let starts =
    nodes |> Map.keys
    |> List.filter ~f:(fun s ->
           Str.string_match (Str.regexp "^[A-Z0-9][A-Z0-9]A$") s 0 )
  in
  let ends_in_z k = Str.string_match (Str.regexp "[A-Z0-9][A-Z0-9]Z$") k 0 in
  let follow d k =
    match (Map.find nodes k, d) with
    | None, _ ->
        k
    | Some n, Dir.L ->
        fst n
    | Some n, Dir.R ->
        snd n
  in
  let rec follow_starts i start =
    if ends_in_z start then i
    else
      follow_starts (i + 1)
        (follow (List.nth_exn dirs (i % List.length dirs)) start)
  in
  let ends = List.map ~f:(follow_starts 0) starts in
  List.fold ~f:lcm ~init:(List.hd_exn ends) (List.tl_exn ends)

let () =
  let result = In_channel.read_all "input" |> parse_map |> Option.value_exn in
  printf "%d\n" @@ (part1 result |> Option.value_exn) ;
  printf "%d\n" @@ part2 result
