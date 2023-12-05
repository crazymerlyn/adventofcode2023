let parse_numbers string = string
  |> String.split_on_char ' '
  |> List.filter (fun (part) -> String.length part > 0)
  |> List.map int_of_string

let print_lines = List.iter print_endline
let print_ints = List.iter (fun (x) -> Printf.printf "%d, " x)
let print_int_pairs pairs = 
  let () = List.iter (fun (x) -> Printf.printf "(%d,%d) " (fst x) (snd x)) pairs in 
  print_newline ()

module Range = struct
  type t = int * int

  let make x y = (x, y)

  let is_empty (x,y) = y > x
  let intersect (x,y) (a,b) = 
    let left = max x a in
    let right = min y b in
    make left (max left right)
end
