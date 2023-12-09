open Myutils

let rec map2 f a b = match a, b with
|x::rest1,y::rest2 -> (f x y) :: map2 f rest1 rest2
|_ -> []

let getdiffs numbers = map2 (-) (List.tl numbers) numbers

let get_next numbers = 
        let rec aux numbers total = match Core.List.all_equal ~equal:(==) numbers with
        | Some x -> x + total
        | None -> aux (getdiffs numbers) (total + (Core.List.last_exn numbers))
        in 
        aux numbers 0

let get_prev numbers = 
        let rec aux numbers total sign = match Core.List.all_equal ~equal:(==) numbers with
        | Some x -> x*sign + total
        | None -> aux (getdiffs numbers) (total + sign*(Core.List.hd_exn numbers)) (sign * -1)
        in 
        aux numbers 0 1

let histories = In_channel.input_lines stdin |> List.map parse_numbers

let () = histories |> List.map get_next |> List.fold_left (+) 0 |> Printf.printf "%d\n"
let () = histories |> List.map get_prev |> List.fold_left (+) 0 |> Printf.printf "%d\n"

