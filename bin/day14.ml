let grid = In_channel.input_lines stdin |> List.map Core.String.to_list

let slide row =
        let rec aux row (os, dots) = match row with
        | [] -> os@dots
        | 'O'::rest -> aux rest (os@['O'], dots)
        | '.'::rest -> aux rest (os, '.'::dots)
        | '#'::rest -> aux rest (os@dots@['#'],[])
        | _::_ -> failwith "invalid string" in
        aux row ([],[])

let slide_west = List.map slide
let slide_east grid = grid
|> List.map List.rev
|> List.map slide
|> List.map List.rev

let slide_north grid = grid
|> Core.List.transpose_exn
|> slide_west
|> Core.List.transpose_exn

let slide_south grid = grid
|> Core.List.transpose_exn
|> slide_east
|> Core.List.transpose_exn

let slide_cycle grid = grid
|> slide_north
|> slide_west
|> slide_south
|> slide_east

let get_score row = 
        let n = List.length row in
        row
        |> List.mapi (fun i x -> if x == 'O' then n-i else 0)
        |> List.fold_left (+) 0

let get_total_load grid = grid
        |> Core.List.transpose_exn
        |> List.map get_score
        |> List.fold_left (+) 0

let revmap h = 
        let h2 = Hashtbl.create (Hashtbl.length h) in
        let () = Hashtbl.iter (fun a b -> Hashtbl.add h2 b a) h in
        h2

let get_map grid =
        let h = Hashtbl.create 1000 in
        let rec aux grid i = match Hashtbl.find_opt h grid with
        | Some(j) -> (revmap h),j,i-j
        | None -> Hashtbl.add h grid i; aux (slide_cycle grid) (i+1) in
        aux grid 0


let cycle_n grid n =
        let h,offset,cycle_length = get_map grid in
        let newn = (n-offset) mod cycle_length + offset in
        Hashtbl.find h newn

let () = Printf.printf "%d\n" (get_total_load (slide_north grid))
let () = Printf.printf "%d\n" (get_total_load (cycle_n grid 1000000000))

