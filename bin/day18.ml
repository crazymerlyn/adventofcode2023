open Myutils

type dig = {dir: dir; steps: int}

let get_next {dir;steps} start = Coordinate.(add (mul (from dir) steps) start)

let area points = Containers.List.rev_map2
        (fun (x1,y1) (x2,y2) -> (x1*y2 - x2*y1, abs (x2-x1) + abs (y2-y1)))
        points
        (List.tl points @ [List.hd points])
        |> List.fold_left (Coordinate.add) (0,0)
        |> fun (x,y) -> (abs x / 2) + (abs y / 2) + 1

let parse_dir1 = function
| "R" -> R
| "L" -> L
| "U" -> U
| "D" -> D
| c -> failwith ("unrecognized dir: " ^ c)

let parse_dir2 = function
| '0' -> R
| '2' -> L
| '3' -> U
| '1' -> D
| c -> failwith (Printf.sprintf "unrecognized dir: %c" c)

let parse_dig1 line = match String.split_on_char ' ' line with
| [dir; steps; _] -> {dir=parse_dir1 dir; steps = int_of_string steps}
| _ -> failwith line

let parse_dig2 line = match String.split_on_char ' ' line with
| [_; _; color] -> {dir=parse_dir2 (String.get color 7); steps = int_of_string ("0x" ^ String.sub color 2 5)}
| _ -> failwith line

let get_shape digs =
        let rec aux start digs result = match digs with
        | [] -> result
        | dig :: rest -> let next = (get_next dig start) in  aux next rest (next::result)
        in
        aux (0,0) digs []

let lines = In_channel.input_lines stdin

let shape = get_shape (List.map parse_dig1 lines)
let shape2 = get_shape (List.map parse_dig2 lines)

let () = Printf.printf "%d\n" (area shape)
let () = Printf.printf "%d\n" (area shape2)

