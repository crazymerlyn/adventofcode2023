type pos = int * int
type dir = E | W | N | S
type light = pos * dir
type cell = Empty | MirrorA | MirrorB | SplitterH | SplitterV

module LS = Set.Make(struct
        type t = light
        let compare = compare
end)

module PS = Set.Make(struct
        type t = pos
        let compare = compare
end)

let parse_cell = function
| '.' -> Empty
| '\\' -> MirrorA
| '/' -> MirrorB
| '|' -> SplitterV
| '-' -> SplitterH
| c -> failwith (Printf.sprintf "Invalid char: %c\n" c)

let parse_grid lines = lines
|> Core.List.concat_mapi ~f:(fun i row -> List.mapi (fun j c -> (i,j),parse_cell c) row)
|> List.to_seq
|> Hashtbl.of_seq

let lines = In_channel.input_lines stdin |> List.map Core.String.to_list

let n = List.length lines
let m = List.length (List.hd lines)

let step dir cell = match dir,cell with
| d,Empty -> [d]

| E,SplitterH | W,SplitterH -> [dir]
| N,SplitterH | S,SplitterH -> [E;W]

| N,SplitterV | S,SplitterV -> [dir]
| E,SplitterV | W,SplitterV -> [N;S]

| E,MirrorA -> [S]
| W,MirrorA -> [N]
| N,MirrorA -> [W]
| S,MirrorA -> [E]

| E,MirrorB -> [N]
| W,MirrorB -> [S]
| N,MirrorB -> [E]
| S,MirrorB -> [W]

let add dir (x,y) = match dir with
| E -> (x,y+1)
| W -> (x,y-1)
| N -> (x-1,y)
| S -> (x+1,y)

let isvalid (x,y) = 0 <= x && x < n && 0 <= y && y < m

let steplight (pos, dir) cell = step dir cell
|> List.map (fun d -> (add d pos, d))
|> List.filter (fun (pos,_) -> isvalid pos)

let not_present_in set v = not (LS.mem v set)

let steps lights grid =
        let rec aux lights grid seen = 
                let unseen = List.filter (not_present_in seen) lights in
                let newlights = List.concat_map (fun (pos,dir) -> steplight (pos,dir) (Hashtbl.find grid pos)) unseen in
                if List.is_empty unseen 
                then seen |> LS.to_list |> List.map fst |> PS.of_list |> PS.cardinal
                else aux newlights grid (LS.add_seq (List.to_seq unseen) seen)
        in
        aux lights grid LS.empty


let from_left = Core.List.range 0 n |> List.map (fun i -> ((i, 0), E))
let from_right = Core.List.range 0 n |> List.map (fun i -> ((i, m-1), W))
let from_top = Core.List.range 0 m |> List.map (fun j -> ((0, j), S))
let from_bottom = Core.List.range 0 m |> List.map (fun j -> ((n-1, j), N))

let starting_lights = from_left @ from_right @ from_top @ from_bottom

let solution2 grid = starting_lights
|> List.map (fun x -> [x])
|> List.map (fun lights -> steps lights grid)
|> Core.List.max_elt ~compare:compare
|> Option.get

let () = Printf.printf "%d\n" (steps [((0,0),E)] (parse_grid lines))
let () = Printf.printf "%d\n" (solution2 (parse_grid lines))

