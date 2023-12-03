open Core
open Re

type number = {
        xmin: int;
        xmax: int;
        y: int;
        value: int;
}

type symbol = {
        x: int;
        y: int;
        text: string;
}

type schematic = {
        numbers: number list;
        symbols: symbol list;
}

let fold_schematic acc schematic = {
        numbers = acc.numbers @ schematic.numbers;
        symbols = acc.symbols @ schematic.symbols;
}

let zero_schematic = {numbers = []; symbols = []}


let parse_number (y : int) (group : Group.t) = 
        let (xmin, xmax) = Group.offset group 0 in
        let value = int_of_string (Group.get group 0) in
        {xmin; xmax; y; value}

let parse_symbol (y: int) (group: Group.t) = 
        {x=(Group.start group 0); y; text=(Group.get group 0)}

let parse_line i line = 
        let groups = Re.all (Pcre.regexp "\\d+|[^.]") line in
        let is_number group = match String.to_list (Group.get group 0) with
        | ('0'..'9') :: _ -> true
        | _ -> false in
        let numgroups,symbolgroups = List.partition_tf ~f:is_number groups in
        let numbers = List.map ~f:(parse_number i) numgroups in
        let symbols = List.map ~f:(parse_symbol i) symbolgroups in
        {numbers; symbols}

let parse lines = lines 
  |> List.mapi ~f:parse_line 
  |> List.fold ~f:fold_schematic ~init:zero_schematic

let is_adjacent {xmin;xmax;y;_} {x=x2;y=y2;_} = xmin - 1 <= x2 && x2 <= xmax && y-1 <= y2 && y2 <= y + 1

let has_adjacent symbols number = symbols
  |> List.exists ~f:(is_adjacent number)

let get_sum schematic = 
        schematic.numbers 
  |> List.filter ~f:(has_adjacent schematic.symbols)
  |> List.map ~f:(fun (number) -> number.value)
  |> List.fold_left ~f:(+) ~init:0

let get_gear_ratio_sum schematic = 
        schematic.symbols
  |> List.filter ~f:(fun (symbol) -> String.equal symbol.text "*")
  |> List.map ~f:(fun (symbol) -> List.filter schematic.numbers ~f:((Fun.flip is_adjacent) symbol))
  |> List.filter ~f:(fun (numbers) -> phys_equal (List.length numbers) 2)
  |> List.map ~f:(fun (numbers) -> match numbers with
          | [x; y] -> x.value * y.value
          | _ -> 0)
  |> List.fold_left ~f:(+) ~init:0

let schematic = parse (In_channel.input_lines In_channel.stdin)

let () = Printf.printf "%d\n" (get_sum schematic)
let () = Printf.printf "%d\n" (get_gear_ratio_sum schematic)

