open Core.List;;

module IntSet = Set.Make(Int)

let parse_numbers string = string
  |> String.split_on_char ' '
  |> List.filter (fun (part) -> String.length part > 0)
  |> List.map int_of_string

let get_matching winning have = let open IntSet in (cardinal (inter (of_list winning) (of_list have)))

let parse_card line = 
        let parts = Core.String.split_on_chars ~on:[':'; '|'] line in
        let winning = parse_numbers (List.nth parts 1) in
        let have = parse_numbers (List.nth parts 2) in
        get_matching winning have

let get_score = function
        | 0 -> 0
        | x -> Core.Int.pow 2 (x-1)

let get_total cards = 
        let cards = Array.of_list cards in
        let copies = Array.make (Array.length cards) 1 in
        for i = 0 to Array.length cards - 1 do
                for j = i+1 to i+cards.(i) do
                        copies.(j) <- copies.(j) + copies.(i)
                done
        done;
        Array.to_list copies


let cards = In_channel.input_lines stdin |> List.map parse_card
let () = Printf.printf "%d\n" (sum ~f:Fun.id (module Core.Int) (List.map get_score cards))
let () = Printf.printf "%d\n" (sum ~f:Fun.id (module Core.Int) (get_total cards))
