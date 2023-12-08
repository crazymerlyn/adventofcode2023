open Myutils

(** parsing *)
let parse_map line = 
        let parts = Core.String.split_on_chars ~on:[' '; '='; ';'; '('; ')'; ','] (String.trim line) in
        (List.nth parts 0,(List.nth parts 4,List.nth parts 6))

let parse_mapper lines = lines
  |> List.map parse_map
  |> List.to_seq
  |> Hashtbl.of_seq

let lines = (In_channel.input_lines stdin) |> List.filter (Fun.negate Core.String.is_empty)

let pattern = (List.hd) lines
let mapper = parse_mapper (List.tl lines)

(** core function to calculate number of steps to reach end *)
let score pattern mapper dest init = 
        let rec aux pattern2 mapper init result = 
                if dest init then result
                else match pattern2 with
                | 'L'::res -> aux res mapper (fst ( Hashtbl.find mapper init )) (result + 1)
                | 'R'::res -> aux res mapper (snd ( Hashtbl.find mapper init )) (result + 1)
                | [] -> aux (Core.String.to_list pattern) mapper init result
                | _ -> failwith "invalid pattern"
        in 
        aux (Core.String.to_list pattern) mapper init 0


(** part 1 *)
let () = Printf.printf "%d\n" (score pattern mapper (String.equal "ZZZ") "AAA")


(** part 2 *)
let endswitha = Hashtbl.to_seq mapper
                |> Seq.map fst
                |> List.of_seq
                |> List.filter (String.ends_with ~suffix:"A")

let scores = List.map (score pattern mapper (String.ends_with ~suffix:"Z")) endswitha


let () = Printf.printf "%d\n" (List.fold_left lcm 1 scores)
