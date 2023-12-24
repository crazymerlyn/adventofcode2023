open Myutils

let grid = In_channel.input_lines stdin 
|> List.map Core.String.to_array
|> Core.List.to_array

let n = Array.length grid
let m = Array.length grid.(0)

let start = (0, 1)
let target = (n-1, m-2)

let isvalid (i,j) = 0 <= i && i < n && 0 <= j && j < m && grid.(i).(j) != '#'
let adj (i,j) = [(i,j+1);(i,j-1);(i+1,j);(i-1,j)]
let adj1 (i,j) = match grid.(i).(j) with
| '>' -> [(i,j+1)]
| '<' -> [(i,j-1)]
| 'v' -> [(i+1,j)]
| '^' -> [(i-1,j)]
| _ -> adj (i,j)

|> List.filter isvalid
let adj2 (i,j) = adj (i,j) |> List.filter isvalid

let compress grid adj =
  let vs = (Core.List.(cartesian_product (range 0 n) (range 0 m))
          |> List.filter (fun (i,j) -> grid.(i).(j) != '#')
          |> List.filter (fun p -> List.length (adj p) > 2))
          @ [start; target]
  in
  let find_adj_intersections v = 
    let rec aux q seen dist result =
      if List.is_empty q then result else
      let aux2 q =
        let alladj = Core.List.(q >>= adj |> filter ~f:(fun k -> not (CS.mem k seen))) in
        List.fold_left 
        (fun (q,s,r) a ->
          if List.mem a vs then (q, CS.add a s, (dist,a)::r)
          else (a :: q, CS.add a s, r))
        ([], seen, result) alladj
      in
      let (newq,newseen,newresult) = aux2 q in
      aux newq newseen (dist+1) newresult
    in
    aux [v] (CS.singleton v) 0 []
  in
  List.map (fun v -> (v,find_adj_intersections v)) vs
  |> List.to_seq |> Hashtbl.of_seq

let dfs graph start target =
  let rec auxdfs start seen curdist =
    if Coordinate.equal start target then curdist else
    let adj = Hashtbl.find graph start in
    let newadj = List.filter (fun (_,v) -> not (CS.mem v seen)) adj in
    newadj
    |> List.map (fun (dist, v) -> auxdfs v (CS.add start seen) (curdist + dist + 1))
    |> Core.List.max_elt ~compare:compare
    |> Option.value ~default:curdist
  in
  auxdfs start CS.empty 0

let () = Printf.printf "%d\n" (dfs (compress grid adj1) start target)

let () = Printf.printf "%d\n" (dfs (compress grid adj2) start target)
