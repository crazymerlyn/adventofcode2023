open Myutils

type pos = Plot | Rock

let parse_pos = function
| '.' | 'S' -> Plot
| '#' -> Rock
| c -> failwith (Printf.sprintf "unrecognized char %c" c)

let parse_row line = line |> Core.String.to_array |> Array.map parse_pos
let parse_grid lines = List.map parse_row lines |> Array.of_list

let find_start_pos lines =
        List.mapi (fun i line -> Option.map (fun j -> (i,j)) (Core.String.index line 'S')) lines
|> List.find (Option.is_some)
|> Option.get

let lines = In_channel.input_lines stdin
let start_pos = find_start_pos lines
let grid = parse_grid lines
let n = Array.length grid
let m = Array.length grid.(0)


let get_surround (i,j) = [(i+1,j);(i-1,j);(i,j-1);(i,j+1)]

let isvalid (i,j) = 0 <= i && i < n && 0 <= j && j < m
let is_plot grid (i, j) =
        isvalid (i,j) && grid.(i).(j) == Plot

let get_next grid start = (get_surround start) |> List.filter (is_plot grid) |> List.to_seq |> CS.of_seq
let get_next_all grid (points, seen) =
        let points = CS.diff points seen in
        (List.fold_right CS.union ( List.map (get_next grid) (CS.to_list points) ) CS.empty) ,(CS.union points seen)


let get_after_steps grid start steps =
        let rec aux (steps,seen) counter =
                if counter == 0
                then CS.union steps seen |> CS.filter (fun (i,j) -> (i+j) mod 2 == 0)
                else aux (get_next_all grid (steps,seen)) (counter - 1)
        in
        aux (CS.singleton start, CS.empty) steps



let solve1 n = (get_after_steps grid start_pos n) |> CS.filter isvalid |> CS.cardinal

let () = Printf.printf "%d\n" (solve1 64)



