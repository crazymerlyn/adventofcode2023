open Myutils

let parse_row line = let parts = String.split_on_char ' ' line in 
        (List.hd parts,
         (List.map int_of_string ( String.split_on_char ',' (List.nth parts 1) )))


let lines = In_channel.input_lines stdin
let rows = List.map parse_row lines
let times5 x = [x;x;x;x;x;]
let expand (field,condition) = (
        String.concat "?" (times5 field),
        List.concat (times5 condition))

let mysub string start length = 
        if length > 0 && start + length <= (String.length string)
        then String.sub string start length
        else ""

let substr string start = mysub string start (String.length string - start)

let solve self (field,condition) = match field,condition with
|"",[] -> 1 
|"",_ -> 0
|x,conds when String.get x 0 == '.' -> self ((substr x 1), conds)
|x,conds when String.get x 0 == '?' -> self ((substr x 1), conds)
                                     + self ((String.cat "#"  (substr x 1)), conds)
|x,[] -> 0
|x,0::rest -> 0
|x,cond::rest -> if String.contains (mysub x 0 cond) '.'
                 then 0
                 else if String.length x < cond then 0
                 else if String.starts_with ~prefix:"#" (substr x cond)
                 then 0
                 else self ((substr x (cond+1)), rest)

let solve_memoized = memo_rec solve
let get_solution rows = rows |> List.map solve_memoized |> List.fold_left (+) 0

let () = Printf.printf "%d\n" (get_solution rows)
let () = Printf.printf "%d\n" (get_solution (List.map expand rows))
