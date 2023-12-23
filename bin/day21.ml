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
let starti,startj = find_start_pos lines
let grid = parse_grid lines
let n = Array.length grid
let m = Array.length grid.(0)


let get_surround (i,j) = [(i+1,j);(i-1,j);(i,j-1);(i,j+1)]

let isvalid (i,j) = 0 <= i && i < n && 0 <= j && j < m
let odd dist = dist mod 2 == 1
let even dist = dist mod 2 == 0

let wrap i n = let i = i mod n in (i + n) mod n
let is_plot grid (i, j) =
         isvalid (i,j) && grid.(i).(j) == Plot

(** Returns a set of points reachable from the given point in one step*)
let get_next grid start = (get_surround start) |> List.filter (is_plot grid) |> List.to_seq |> CS.of_seq

(** Takes in a set of points and a set of points already seen.
    Returns a set of newpoints reachable from the ones given and an updated seen set
 *)
let get_next_all grid (points, seen) =
        let newseen = CS.union points seen in
        let newpoints = (List.fold_right CS.union ( List.map (get_next grid) (CS.to_list points) ) CS.empty) in 
        let newpoints = CS.diff newpoints seen in
        newpoints,newseen


(** Returns list of points and distance from the start after given steps *)
let get_after_steps grid start steps =
        let rec aux (points,seen) result counter =
                if counter == steps
                then result
                else
                        let newpoints,newseen = (get_next_all grid (points,seen)) in
                        let pointswithdist = newpoints |> CS.to_list |> List.map (fun p -> (p,counter+1)) in
                        aux (newpoints,newseen) (result @ pointswithdist) (counter + 1)
        in
        aux (CS.singleton start, CS.empty) [(start,0)] 0


let solve1 steps = (get_after_steps grid (starti,startj) steps)
|> List.filter (fun (_,dist) -> dist mod 2 == steps mod 2)
|> List.length

let solve2 steps =
        let x = (steps - n/2) / n in
        let visited = get_after_steps grid (starti,startj) n |> List.map snd in
        let odd_full = List.filter odd visited in
        let even_full = List.filter even visited in
        let odd_corners = List.filter (fun x -> x > n/2) odd_full in
        let even_corners = List.filter (fun x -> x > n/2) even_full in
        (x + 1) * (x+1) * (List.length odd_full)
        + x * x * (List.length even_full)
        - (x+1) * (List.length odd_corners)
        + x * (List.length even_corners)

let () = Printf.printf "%d\n" (solve1 64)
let () = Printf.printf "%d\n" (solve2 26501365)



