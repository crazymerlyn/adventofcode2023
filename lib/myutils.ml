let parse_numbers string = string
  |> String.split_on_char ' '
  |> List.filter (fun (part) -> String.length part > 0)
  |> List.map int_of_string

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
end
