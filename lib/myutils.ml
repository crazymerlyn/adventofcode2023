let parse_numbers string = string
  |> String.split_on_char ' '
  |> List.filter (fun (part) -> String.length part > 0)
  |> List.map int_of_string

let print_lines = List.iter print_endline
let print_ints ints = let () = List.iter (fun (x) -> Printf.printf "%d, " x) ints in
  Printf.printf "\n"
let print_floats floats = let () = List.iter (fun (x) -> Printf.printf "%f, " x) floats in
  Printf.printf "\n"
let print_int_pairs pairs = 
  let () = List.iter (fun (x) -> Printf.printf "(%d,%d) " (fst x) (snd x)) pairs in 
  Printf.printf "\n"

module Range = struct
  type t = int * int

  let make x y = (x, y)

  let is_empty (x,y) = y > x
  let intersect (x,y) (a,b) = 
    let left = max x a in
    let right = min y b in
    make left (max left right)
end
