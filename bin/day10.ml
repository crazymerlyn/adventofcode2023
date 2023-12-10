module IntPairs = 
        struct
         type t = int * int
         let compare (x0,y0) (x1,y1) =
           match Stdlib.compare x0 x1 with
               0 -> Stdlib.compare y0 y1
             | c -> c
        end

module PS = Set.Make(IntPairs)

let mat = In_channel.input_lines stdin |> List.map Core.String.to_list

let n = List.length mat
let m = List.length (List.nth mat 0)

let get_start_pos mat = 
        let aux = List.find_mapi (fun i x -> if x == 'S' then Some i else None) in
        List.find_mapi (fun i row -> match aux row with
        | Some j -> Some (i, j)
        | None -> None) mat


let undirected a b = [(a, b); (b, a)]
let rec keepdouble = function
        | x :: rest -> if compare (Some x) (Core.List.hd rest) == 0
        then x :: keepdouble (List.tl rest) 
        else keepdouble rest
        | [] -> []
let getedgesrow i = Core.List.concat_mapi ~f:(fun j c -> match c with
| '|' -> undirected (i-1,j) (i,j) @ undirected (i, j) (i+1, j)
| '-' -> undirected (i,j-1) (i,j) @ undirected (i, j) (i, j+1)
| 'L' -> undirected (i-1,j) (i,j) @ undirected (i, j) (i, j+1)
| 'J' -> undirected (i-1,j) (i,j) @ undirected (i, j) (i, j-1)
| '7' -> undirected (i+1,j) (i,j) @ undirected (i, j) (i, j-1)
| 'F' -> undirected (i+1,j) (i,j) @ undirected (i, j) (i, j+1)
| 'S' -> undirected (i-1,j) (i,j) @ undirected (i, j) (i, j+1) @
         undirected (i+1,j) (i,j) @ undirected (i, j) (i, j-1)
| _ -> [])

let getedges mat = Core.List.concat_mapi ~f:getedgesrow mat
|> List.sort compare
|> keepdouble

let getadjlist edges = 
        let h = Hashtbl.create (n*m) in
        let () = List.iter (fun (a, b) -> 
                Hashtbl.replace h a (match Hashtbl.find_opt h a with
                        | Some adj -> b :: adj
                        | None -> [b]
                )) edges in
        h


let getloop start edges = 
        let rec aux start edges seen = 
                if PS.mem start seen
        then seen
        else match Hashtbl.find_opt edges start with
        | Some(adj1 :: adj2 :: []) -> if PS.mem adj1 seen 
                                then if PS.mem adj2 seen 
                                        then (PS.add start seen)
                                        else aux adj2 edges (PS.add start seen)
                                else aux adj1 edges (PS.add start seen)
        | Some(_) -> failwith "error"
        | _ -> failwith (Printf.sprintf "%d,%d" (fst start) (snd start))
        in
        aux start edges PS.empty


let start_pos = Option.get (get_start_pos mat)
let edges = getedges mat
let adjlist = getadjlist edges


let loop = getloop start_pos adjlist

let join a b = (a,b)
let inside = List.mapi 
(fun i row -> 
        Core.List.fold_left (List.mapi join row) ~init:(0,0) ~f:(fun (bars, result) (j,c) -> 
                if PS.mem (i, j) loop then
                match c with
                | 'J' | '|' | 'S' | 'L' -> (bars+1, result)
                | _ -> (bars, result)
                else 
                        if bars mod 2 == 1 then (bars, result + 1) else (bars, result)))
mat
|> List.map snd
|> List.fold_left (+) 0



let () = Printf.printf "%d\n" ((PS.cardinal loop) / 2)
let () = Printf.printf "%d\n" (inside)

