let lines = In_channel.input_lines stdin |> List.map Core.String.to_list

let get_offset n line = if (Core.List.mem line '#' ~equal:(==)) then 1 else n

let get_map n lines =
        let hs = List.map (get_offset n) lines 
        |> Core.List.folding_map ~init:0 ~f:(fun acc a -> (acc+a,acc))
        |> Array.of_list
        in
        let vs = List.map (get_offset n) (Core.List.transpose_exn lines)
        |> Core.List.folding_map ~init:0 ~f:(fun acc a -> (acc+a,acc))
        |> Array.of_list
        in
        (hs, vs)

let get_galaxies n lines =
        let (hs,vs) = get_map n lines in
        Core.List.concat_mapi ~f:(fun i line -> 
        Core.List.filter_mapi ~f:(fun j c -> if c == '#' then Some((hs.(i),vs.(j))) else None) line)
        lines

let get_dist ((x1,y1),(x2,y2)) = abs (x1-x2) + abs (y1-y2)
let get_dists galaxies = galaxies 
        |> List.concat_map (fun (g1) -> List.map (fun (g2) -> (g1,g2)) galaxies)
        |> List.map get_dist
let get_totaldist galaxies = galaxies
        |> get_dists
        |> List.fold_left (+) 0
        |> (Fun.flip (/)) 2


let () = Printf.printf "%d\n" (get_totaldist (get_galaxies 2 lines))
let () = Printf.printf "%d\n" (get_totaldist (get_galaxies 1000000 lines))
