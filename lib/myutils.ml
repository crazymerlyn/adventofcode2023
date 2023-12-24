let parse_numbers string = string
  |> String.split_on_char ' '
  |> List.filter (fun (part) -> String.length part > 0)
  |> List.map int_of_string

let getgroups lines = 
  let rec aux lines result = match lines with
  | [] -> result
  | []::rest -> aux rest ([]::result)
  | x::rest -> aux rest ((x::(List.hd result)) :: (List.tl result))
  in 
  aux lines [[]] 
  |> List.map List.rev 
  |> List.rev 
  |> List.filter (Fun.negate List.is_empty)

let print_pair (x,y) = Printf.printf "(%d, %d)" x y
let print_lines = List.iter print_endline
let print_ints ints = let () = List.iter (fun (x) -> Printf.printf "%d, " x) ints in
  Printf.printf "\n"
let print_floats floats = let () = List.iter (fun (x) -> Printf.printf "%f, " x) floats in
  Printf.printf "\n"
let print_int_pairs pairs = 
  let () = List.iter (fun (x) -> Printf.printf "(%d,%d) " (fst x) (snd x)) pairs in 
  Printf.printf "\n"


let rec gcd a (b:int) = if a == 0 then b else gcd (b mod a) a
let lcm a b = a * b / (gcd a b)

let memo_rec f ?(cachesize=100) =
  let m = Hashtbl.create cachesize in
  let rec g x =
    try
      Hashtbl.find m x
    with
    Not_found ->
      let y = f g x in
        Hashtbl.add m x y ;
        y
  in
    g

module Range = struct
  type t = int * int

  let make x y = (x, y)

  let is_empty (x,y) = y > x
  let intersect (x,y) (a,b) = 
    let left = max x a in
    let right = min y b in
    make left (max left right)

  let intersects (x1,y1) (x2,y2) = max x1 x2 <= min y1 y2 
end

type dir = R | L | U | D [@@deriving show]
module Coordinate = struct
  type t = int * int [@@deriving show]
  let equal (i1,j1) (i2, j2) = i1 == i2 && j1 == j2

  let add (i1,j1) (i2, j2) = (i1+i2, j1+j2)
  let compare (i1,j1) (i2,j2) =
    if i1 < i2 then -1 else if i1 > i2 then 1 else (compare j1 j2)

  let from : dir -> t = function
  | R -> (0, 1)
  | L -> (0, -1)
  | U -> (-1, 0)
  | D -> (1, 0)

  let mul (i, j) k = (i*k, j*k)
end

module CS = Set.Make(Coordinate)

module Grid = struct
  type 'a t = 'a array array

  let mem t (j, i) = 0 <= i && i < Array.length t && 0 <= j && j < Array.length t.(i)
  let get t (j, i) = if mem t (j,i) then Some (t.(i).(j)) else None
end


type 'a vertex = {
  id: 'a;
  mutable visited: bool;
  mutable neighbours: 'a vertex list;
}

let get_vertices edges vertices = 
  let h = List.map 
  (fun v -> 
    v,{id = v;
    visited = false;
    neighbours = []}) 
  vertices
  |> List.to_seq
  |> Hashtbl.of_seq
  in
  let () = Hashtbl.iter (fun v v' -> 
    v'.neighbours <- (edges v) |> List.map (Hashtbl.find h)
  ) h
  in
  Hashtbl.to_seq_values h |> List.of_seq



let rec dfs vertex stack =
  vertex.visited <- true;
  List.iter (fun neighbor ->
    if not neighbor.visited then dfs neighbor stack
  ) vertex.neighbours;
  stack := vertex.id :: !stack

let topological_sort vertices =
  let stack = ref [] in
  List.iter (fun vertex ->
    if not vertex.visited then dfs vertex stack
  ) vertices;
  List.rev !stack
