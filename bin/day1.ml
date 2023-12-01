let lines = Core.In_channel.input_lines stdin

let isdigit : (char -> bool) = function 
        | '0' .. '9' -> true
        | _ -> false
let digits s = List.filter isdigit (Core.String.to_list s)

let regreedy = Re.Pcre.regexp ".*([0-9]|one|two|three|four|five|six|seven|eight|nine)"
let re = Re.Pcre.regexp ".*?([0-9]|one|two|three|four|five|six|seven|eight|nine)"

let first_matched_group re string = Re.Group.get (Re.exec re string) 1

let digit_of_string s = match s with
        | "one" -> '1'
        | "two" -> '2'
        | "three" -> '3'
        | "four" -> '4'
        | "five" -> '5'
        | "six" -> '6'
        | "seven" -> '7'
        | "eight" -> '8'
        | "nine" -> '9'
        | _ -> String.get s 0

let digits2 string = List.map digit_of_string [(first_matched_group re string); (first_matched_group regreedy string)]

let first_and_last s = Core.String.of_list (List.hd s :: [List.hd (List.rev s)])

let result1 = List.fold_left (+) 0 (List.map (fun x -> int_of_string (first_and_last (digits x))) lines)
let result2 = List.fold_left (+) 0 (List.map (fun x -> int_of_string (first_and_last (digits2 x))) lines)

let () = print_endline (string_of_int result1)
let () = print_endline (string_of_int result2)


