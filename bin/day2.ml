(* Define the data types *)
type cube_subset = { red: int; green: int; blue: int }
type game = { id: int; subset: cube_subset }

let zero_subset = {red = 0; green = 0; blue = 0}
let onsubset func acc {red; green; blue} = {red=func acc.red red; green=func acc.green green; blue=func acc.blue blue}
let sumsubsets = List.fold_left (onsubset (+)) zero_subset
let maxsubsets = List.fold_left (onsubset max) zero_subset

let parse_part part_str = match (String.split_on_char ' ' (String.trim part_str)) with
     | [x; "red"] -> {zero_subset with red = int_of_string x}
     | [x; "green"] -> {zero_subset with green = int_of_string x}
     | [x; "blue"] -> {zero_subset with blue = int_of_string x}
     | y -> raise (Invalid_argument (String.concat " " y))

let parse_subset subset_str =
        let parts = String.split_on_char ',' subset_str in
        sumsubsets (List.map parse_part parts)

let parse_game line = match String.split_on_char ':' line with
     | [head; rest] -> 
                     let id =  int_of_string (List.nth (String.split_on_char ' ' head) 1) in
                     let subset = maxsubsets (List.map parse_subset (String.split_on_char ';' rest)) in
                     { id; subset }
     | _ -> raise (Invalid_argument "")


let is_possible game =
        game.subset.red <= 12 && game.subset.green <= 13 && game.subset.blue <= 14

let sum_possible_games games =
        List.fold_left (fun acc game -> if is_possible game then acc + game.id else acc) 0 games

let power_of_game game =
        game.subset.red * game.subset.green * game.subset.blue


let input = (In_channel.input_lines stdin)

let result1 =
        let games = List.map parse_game input in
        sum_possible_games games

let result2 = 
        let games = List.map parse_game input in
        List.fold_left (fun acc game -> acc + power_of_game game) 0 games;;


print_endline (string_of_int result1);;
print_endline (string_of_int result2);;
