type status = On | Off
type signal = Low | High [@@deriving eq]

type state = 
        BroadcasterState
        | FlipFlopState of status
        | ConjunctionState of (string, signal) Hashtbl.t

type module' = 
        Broadcaster of string list
        | FlipFlop of string * (string list)
        | Conjunction of string * (string list)

type pulse = {source: string; dest: string; signal: signal}
[@@deriving eq]

let get_name = function
        | Broadcaster _ -> "broadcaster"
        | FlipFlop (n,_) -> n
        | Conjunction (n,_) -> n

let dests = function
        | Broadcaster ns -> ns
        | FlipFlop (_,ns) -> ns
        | Conjunction (_,ns) -> ns

let add signal (x,y) = if signal == Low then (x+1,y) else (x,y+1)
let flip = function
| Off -> On
| On -> Off

let get_sources modules name = modules
        |> List.filter (fun module' -> List.mem name (dests module'))
        |> List.map get_name

let signalofswitch = function
| On -> High
| Off -> Low

let getsignal s = Hashtbl.to_seq_values s 
|> Seq.for_all ((==) High)
|> (fun b -> if b then Low else High)

let broadcast source signal = List.map (fun n -> {source; dest=n; signal})
let default_state modules = function
        | Broadcaster _ -> BroadcasterState
        | FlipFlop _ -> FlipFlopState Off
        | Conjunction (n,_) -> ConjunctionState (
                get_sources modules n 
                |> List.map (fun name -> (name, Low)) 
                |> List.to_seq 
                |> Hashtbl.of_seq)

let parse_module' line = 
        let parts = Core.String.substr_replace_all ~pattern:" -> " ~with_:">" line
                |> String.split_on_char '>'
        in
        (List.hd parts, List.nth parts 1 |> String.split_on_char ',' |> List.map Core.String.strip)

let parse_module line = 
        match String.get line 0 with
        | '%' -> let (x,y) = (parse_module' (String.sub line 1 (String.length line - 1))) in FlipFlop (x,y)
        | '&' -> let (x,y) = (parse_module' (String.sub line 1 (String.length line - 1))) in Conjunction (x,y)
        | _ -> let (_,y) = (parse_module' line) in Broadcaster y

let parse_modules lines =
        let h = Hashtbl.create (List.length lines) in
        let modules = List.map parse_module lines in
        let () = List.iter (fun (module') -> 
                Hashtbl.add h (get_name module') (module',default_state modules module')
        ) modules in
        h


let rec aux modules pulses res rxpulse = match pulses with
| [] -> Some(res)
| {source;dest;signal} :: rest -> 
                if equal_pulse rxpulse {source;dest;signal} then None else
                let res = add signal res in
                match Hashtbl.find_opt modules dest with
        | Some(Broadcaster ns, _) -> aux modules (rest @ (broadcast "broadcaster" signal ns)) res rxpulse
        | Some(FlipFlop (n,ns), FlipFlopState s) -> 
                        if signal == High then
                                aux modules rest res rxpulse
else let () = Hashtbl.replace modules dest
                        (FlipFlop (n,ns), FlipFlopState (flip s)) in
aux modules (rest @ broadcast n (signalofswitch (flip s)) ns) res rxpulse
        | Some(Conjunction (n,ns), ConjunctionState s) ->
                        let () = Hashtbl.replace s source signal in
                        aux modules (rest @ broadcast n (getsignal s) ns) res rxpulse
        | Some(_, _) -> failwith "wrong combo"
        | None -> aux modules rest res rxpulse

let init_pulse = {source="button";dest="broadcaster";signal=Low}

let rec gcd a b = if a == 0 then b else gcd (b mod a) a
let lcm a b = a * b / (gcd a b)

let run modules =
        let res = (0, 0) in
        List.fold_left (fun res _ -> aux modules [init_pulse] res {init_pulse with source="abcds";} |> Option.get) res (Core.List.range 0 1000)



let run2 lines =
        let modules = parse_modules lines in
        let p = List.hd (get_sources (Hashtbl.to_seq_values modules |> Seq.map fst |> List.of_seq) "rx") in
        let ps = get_sources (Hashtbl.to_seq_values modules |> Seq.map fst |> List.of_seq) p in
        let rec loop modules res k counter = match res with
        | Some(res) -> loop modules (aux modules [init_pulse] (res) {source=k;dest=p;signal=High} ) k (counter + 1)
        | None -> counter in
        List.map
        (fun k -> loop (parse_modules lines) (Some (0,0)) k 0)
        ps
        |> List.fold_left lcm 1

let lines = In_channel.input_lines stdin

let () = Printf.printf "%d\n" (let (x,y) = run (parse_modules lines) in x * y)
let () = Printf.printf "%d\n" (run2 lines)


