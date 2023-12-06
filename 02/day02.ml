open Stdio

type set = {red: int; green: int; blue: int} ;;
type game = {id: int; sets: set list} ;;

let empty_set = {red=0; green=0; blue=0} ;;

let set_color set color =
    let parts = String.split_on_char ' ' color in
    let num = int_of_string (List.hd parts) in
    match (List.nth parts 1) with
        | "red" -> {set with red=set.red + num}
        | "blue" -> {set with blue=set.blue + num}
        | _ -> {set with green=set.green + num} ;;


let parse_set str = str
    |> Str.split (Str.regexp ", ") 
    |> List.fold_left set_color empty_set ;;

let parse_sets str = str 
    |> Str.split (Str.regexp "; ") 
    |> List.map parse_set ;;

let game_r = Str.regexp {|Game \([0-9]+\): \(.*\)|}

let parse_game line =
    let _ = Str.string_match game_r line 0 in
    let id = int_of_string (Str.matched_group 1 line) in
    let sets = parse_sets (Str.matched_group 2 line) in
    { id; sets; } ;;

let is_possible values game =
    let set_possible x = values.red >= x.red && values.blue >= x.blue && values.green >= x.green in
    List.for_all set_possible game.sets ;;

let max_set a b =
    {red=(max a.red b.red); green=(max a.green b.green); blue=(max a.blue b.blue); } ;;

let power_of_game game = 
    let maximums = List.fold_left max_set empty_set game.sets in
    maximums.red * maximums.green * maximums.blue ;;

let part1 games = games
    |> List.filter (is_possible {red=12; green=13; blue=14}) 
    |> List.map (fun x -> x.id)
    |> List.fold_left (+) 0 ;;

let part2 games = games
    |> List.map power_of_game 
    |> List.fold_left (+) 0 ;;
    
let () =
    let games = In_channel.read_all "input" 
        |> String.trim
        |> String.split_on_char '\n'
        |> List.map parse_game in
    printf "%d\n" @@ (part1 games);
    printf "%d\n" @@ (part2 games) ;;
