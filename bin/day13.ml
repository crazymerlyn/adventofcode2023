open Myutils

let lines = In_channel.input_lines stdin |> List.map Core.String.to_list
let grids = getgroups lines

let diff1 a b = (List.map2 (fun x y -> if x != y then 1 else 0) a b |> List.fold_left (+) 0) == 1

let rec part_equal a b = match a,b with
| [], _ -> true
| _, [] -> true
| x::resta, y::restb -> List.equal (==) x y && part_equal resta restb

let rec smudge_equal a b = match a,b with
| [], _ -> false
| _, [] -> false
| x::resta, y::restb -> diff1 x y && part_equal resta restb
                      || List.equal (==) x y && smudge_equal resta restb

let get_reflect grid equal_func =
        let rec aux left right result = match right with
        | [] -> result
        | x :: rest -> if equal_func left right
                then aux (x::left) rest (max result (List.length left))
                else aux (x::left) rest result
        in aux [] grid 0

let get_score equal_func grid = max (100 * get_reflect grid equal_func)
                                    (get_reflect (Core.List.transpose_exn grid) equal_func)

let () = Printf.printf "%d\n" (List.map (get_score part_equal) grids |> List.fold_left (+) 0)
let () = Printf.printf "%d\n" (List.map (get_score smudge_equal) grids |> List.fold_left (+) 0)

