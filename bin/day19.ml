type cond = {name: char; comp: (int -> bool)}

let always_true = {name='x'; comp = Fun.const true}
let parse_cond string = 
        let target = int_of_string (String.sub string 2 (String.length string - 2)) in
        if String.get string 1 == '>' then
                {name= String.get string 0 ; comp=(fun (x) -> x > target)}
        else
                {name= String.get string 0 ; comp=(fun (x) -> x < target)}

type flow = {cond: cond; target: string}
let parse_flow string =
        if Core.String.mem string ':'
        then let parts = String.split_on_char ':' string in
        {cond = parse_cond (List.nth parts 0); target = (List.nth parts 1)}
        else {cond = always_true; target = string}

type workflow = flow list

let parse_workflow string : workflow = string
|> String.split_on_char ','
|> List.map parse_flow

let parse_workflows lines =
        let h = Hashtbl.create (List.length lines) in
        let () = List.iter (fun (line) -> 
                let parts = Core.String.split_on_chars ~on:['}'; '{'] line in
                Hashtbl.add h (List.nth parts 0) (parse_workflow (List.nth parts 1))
        ) lines in
        h

let parse_part string =
        let h = Hashtbl.create 4 in
        let () = String.sub string 1 (String.length string - 2)
        |> String.split_on_char ','
        |> List.iter (fun assign -> let parts = String.split_on_char '=' assign in
                        Hashtbl.add h (String.get assign 0) (int_of_string (List.nth parts 1))) in
        h

let lines = In_channel.input_lines stdin
let workflows, parts =
        let rec aux lines res = match lines with
        | "" :: rest -> parse_workflows (List.rev res), List.map parse_part rest
        | x :: rest -> aux rest (x :: res)
        | _ -> failwith "error" in
        aux lines []

let run_cond {name; comp} part = comp (Hashtbl.find part name)

let rec run_workflow w part = match w with
| [] -> failwith "not possible"
| {cond;target} :: rest -> if run_cond cond part then target else run_workflow rest part

let run_workflows h part =
        let rec aux flow part = match flow with
        | "R" | "A" -> flow
        | x -> aux (run_workflow (Hashtbl.find h x) part) part
        in aux "in" part

let ans = parts 
        |> List.filter (fun part -> String.equal (run_workflows workflows part) "A")
        |> List.map (fun part -> Hashtbl.fold (fun _ v acc -> acc + v) part 0)
        |> List.fold_left (+) 0

let part2 = "xmas" |> Core.String.to_list |> List.map (fun s -> (s, Core.List.range 1 4001)) |> List.to_seq |> Hashtbl.of_seq

let run_cond2 {name; comp} part = let newpart = Hashtbl.copy part in
        let () = Hashtbl.replace newpart name (List.filter comp (Hashtbl.find part name)) in
        let () = Hashtbl.replace part name (List.filter (Fun.negate comp) (Hashtbl.find part name)) in
        newpart, part

let rec run_workflow2 w part = match w with
| [] -> []
| {cond;target} :: rest -> 
                let newpart, part = run_cond2 cond part in 
                (target,newpart) :: run_workflow2 rest part

let run_workflows2 h part =
        let rec aux flow res = match flow with
        | [] -> res
        | ("A", part):: rest -> aux rest (part :: res)
        | ("R", _):: rest -> aux rest res
        | (x,part):: rest -> aux (rest @ (run_workflow2 (Hashtbl.find h x) part)) res
        in aux [("in", part)] []

let ans2 = run_workflows2 workflows part2
        |> List.map (fun part -> Hashtbl.fold (fun _ v acc -> acc * List.length v) part 1)
        |> List.fold_left (+) 0

let () = Printf.printf "%d\n" ans
let () = Printf.printf "%d\n" ans2

