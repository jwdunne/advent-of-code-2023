open Stdio
open Base

module Card = struct
  module T = struct
    type t = N of int | T | J | Q | K | A | JK

    type cards = t list

    let as_int = function
      | A ->
          14
      | K ->
          13
      | Q ->
          12
      | J ->
          11
      | T ->
          10
      | N a ->
          a
      | JK ->
          1

    let compare a b = compare_int (as_int a) (as_int b)

    let equal a b = compare a b = 0

    let sexp_of_t a = a |> as_int |> Int.sexp_of_t

    let of_char ?(joker = false) = function
      | 'A' ->
          A
      | 'K' ->
          K
      | 'Q' ->
          Q
      | 'J' -> if joker then JK else J
      | 'T' ->
          T
      | '0' .. '9' as c ->
          N (Char.get_digit_exn c)
      | c ->
          raise (Failure ("Cannot convert " ^ String.of_char c ^ " to Card"))
  end

  include T
  include Comparator.Make (T)
end

module Hand = struct
  module T = struct
    type cards = Card.cards

    type t =
      | Flush of cards
      | FourKind of cards
      | FullHouse of cards
      | ThreeKind of cards
      | TwoPair of cards
      | Pair of cards
      | HighCard of cards

    let is_kind n (cs : cards) =
      let uniques = List.find_all_dups cs ~compare:Card.compare in
      List.exists ~f:(fun c -> List.count cs ~f:(Card.equal c) = n) uniques

    let is_full_house (cs : cards) = is_kind 3 cs && is_kind 2 cs

    let is_two_pair (cs : cards) =
      List.dedup_and_sort cs ~compare:Card.compare
      |> List.map ~f:(fun c -> List.count cs ~f:(Card.equal c))
      |> List.filter ~f:(equal 2)
      |> List.length |> ( = ) 2

    let count_jokers =
      List.count ~f:(Card.equal Card.JK)

    let of_cards (cs : cards) =
      let jks = count_jokers cs in
      match cs with
      | a when is_kind 5 a ->
          Flush a
      | a when is_kind 4 a ->
          if jks > 0 then Flush a else FourKind a
      | a when is_full_house a ->
          if jks = 2 || jks = 3 then Flush a else FullHouse a
      | a when is_kind 3 a ->
          if jks = 1 || jks = 3 then FourKind a
          else ThreeKind a
      | a when is_two_pair a ->
          if jks = 2 then FourKind a
          else if jks = 1 then FullHouse a
          else TwoPair a
      | a when is_kind 2 a ->
          if jks = 1 || jks = 2 then ThreeKind a else Pair a
      | a ->
          if jks = 1 then Pair a else HighCard a

    let card = function
      | Flush a ->
          a
      | FourKind a ->
          a
      | FullHouse a ->
          a
      | ThreeKind a ->
          a
      | TwoPair a ->
          a
      | Pair a ->
          a
      | HighCard a ->
          a

    let to_int = function
      | Flush _ ->
          6
      | FourKind _ ->
          5
      | FullHouse _ ->
          4
      | ThreeKind _ ->
          3
      | TwoPair _ ->
          2
      | Pair _ ->
          1
      | HighCard _ ->
          0

    let rec compare_hands c1 c2 =
      match (c1, c2) with
      | [], [] ->
          0
      | [], _ :: _ ->
          0
      | _ :: _, [] ->
          0
      | h1 :: t1, h2 :: t2 -> (
        match Card.compare h1 h2 with
        | x when x > 0 ->
            1
        | x when x < 0 ->
            -1
        | _ ->
            compare_hands t1 t2 )

    let compare a b =
      let cmp = compare_int (to_int a) (to_int b) in
      if cmp = 0 then compare_hands (card a) (card b) else cmp

    let name = function
      | Flush _ ->
          "Flush"
      | FourKind _ ->
          "Four of a Kind"
      | FullHouse _ ->
          "Full House"
      | ThreeKind _ ->
          "Three of a Kind"
      | TwoPair _ ->
          "Two Pair"
      | Pair _ ->
          "Pair"
      | HighCard _ ->
          "High Card"

    let sexp_of_t a = Sexp.Atom (name a)

    let of_string ?(joker = false) s =
      s |> String.to_list
      |> List.map ~f:(Card.of_char ~joker)
      |> of_cards 
  end

  include T
  include Comparator.Make (T)
end

module Bet = struct
  module T = struct
    type t = Hand.t * int

    let compare (h1, _) (h2, _) = Hand.compare h1 h2

    let sexp_of_t (h, b) = Sexp.List [Hand.sexp_of_t h; Int.sexp_of_t b]

    let of_string ?(joker = false) s =
      let parts = String.split ~on:' ' s in
      ( Hand.of_string ~joker (List.hd_exn parts)
      , Int.of_string (List.last_exn parts) )
  end

  include T
  include Comparator.Make (T)
end

let solve ?(joker = false) lines =
  lines
  |> List.map ~f:(Bet.of_string ~joker)
  |> List.sort ~compare:Bet.compare
  |> List.foldi ~init:0 ~f:(fun i acc (_, bet) -> acc + (bet * (i + 1)))

let part1 = solve ~joker:false

let part2 = solve ~joker:true

let () =
  let lines = In_channel.read_all "input" |> String.split_lines in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines
