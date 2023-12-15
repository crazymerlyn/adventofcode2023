type op = Delete of string | Add of string * int

type box = (string * int) list
type boxes = (int * box) list

let empty_boxes : boxes = Core.List.range 0 256 |> List.map (fun i -> (i,[]))

let delete_str string = List.filter (fun (str,_) -> not (String.equal string str))
let has_str string box = box
|> List.filter (fun (str,_) -> String.equal string str)
|> Fun.negate List.is_empty

let add_str string focus box = if has_str string box
then box
     |> List.map (fun (str,f) -> if String.equal string str then (string,focus) else (str,f))
else (string,focus)::box

let lines = In_channel.input_lines stdin

let get_hash = String.fold_left (fun acc ch -> (acc + Char.code ch) * 17 mod 256) 0

let apply_op boxes = function
| Delete string -> let h = get_hash string in
           boxes 
        |> List.map (fun (i,box) -> if i <> h then (i,box) else (i, delete_str string box))
| Add (string,focus) -> let h = get_hash string in
           boxes 
        |> List.map (fun (i,box) -> if i <> h then (i,box) else (i, add_str string focus box))


let get_lens_score i (_,focus) = (i+1) * focus
let get_box_score (i,box) = (i+1) * (List.fold_left (+) 0 (List.mapi get_lens_score (List.rev box)))


let get_score line = line
|> String.split_on_char ','
|> List.map get_hash
|> List.fold_left (+) 0


let parse_op string =
        let n = String.length string in
        match String.get string (n-1) with
        | '-' -> Delete (String.sub string 0 (n-1))
        | _ -> Add ((String.sub string 0 (n-2)), (int_of_string (String.sub string (n-1) 1)))

let get_score2 line = line
|> String.split_on_char ','
|> List.map parse_op
|> List.fold_left apply_op empty_boxes
|> List.map get_box_score
|> List.fold_left (+) 0

let () = Printf.printf "%d\n" (get_score (List.hd lines))
let () = Printf.printf "%d\n" (get_score2 (List.hd lines))
