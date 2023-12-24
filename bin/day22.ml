open Myutils

type point = {x:int; y:int; z:int} [@@deriving show,eq]
type brick = {low: point; high: point} [@@deriving show,eq]

let parse_point string =
        let parts = String.split_on_char ',' string |> List.map int_of_string in
        {x=(List.nth parts 0);y=(List.nth parts 1);z=(List.nth parts 2)}

let parse_brick line =
        let parts = String.split_on_char '~' line in
        {low = parse_point (List.nth parts 0); high = parse_point (List.nth parts 1)}

let sortbricks = List.sort (fun {low={z=z1}} {low={z=z2}} -> compare z1 z2)

let getsquare {low;high} = ((low.x,low.y), (high.x,high.y))
let intersects ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) = 
        Range.intersects (x1, x2) (x3, x4)
        && Range.intersects (y1, y2) (y3, y4)

let blocked_by b1 b2 = b1.high.z == (b2.low.z-1) && intersects (getsquare b1) (getsquare b2)
let blocks b1 b2 = blocked_by b2 b1

let is_at_rest brick bricks =
        brick.low.z == 1 ||
        List.exists (blocks brick) bricks 

let dropbrick bricks brick = 
        let z = bricks
        |> List.filter (fun brick2 ->intersects (getsquare brick) (getsquare brick2))  
        |> List.map (fun b -> b.high.z + 1)
        |> Core.List.max_elt ~compare:compare
        |> Option.value ~default:1 in
        let diff = brick.low.z - z in
        {low={brick.low with z};  high={brick.high with z=brick.high.z - diff}} :: bricks

let count_falling at_rest falling =
        let rec aux at_rest falling result = match falling with
        | [] -> result
        | x :: still_falling ->
                        let new_at_rest = dropbrick at_rest x in
                        let fall = x.low.z != (List.hd new_at_rest).low.z in
                        aux new_at_rest still_falling (result + if fall then 1 else 0)
        in
        aux at_rest falling 0



let supported_by bricks = 
        let h = Hashtbl.create (List.length bricks) in
        let () = List.iter (fun brick -> 
                Hashtbl.add h brick (Core.List.count ~f:(blocks brick) bricks)
        ) bricks in
        h

let will_fall bricks =
        let s = supported_by bricks in
        let rec aux bricks result = match bricks with
        | [] -> result
        | brick :: bricks -> 
                let h = List.map (fun b -> (b,Hashtbl.find s b)) bricks |> List.to_seq |> Hashtbl.of_seq in
                let rec aux2 todrop result = match todrop with
                | [] -> result
                | b :: todrop -> let bs = List.filter (blocked_by b) bricks in
                                 let () = List.iter (fun b -> Hashtbl.replace h b (Hashtbl.find h b - 1)) bs in
                                 aux2 (todrop @ (List.filter (fun b -> Hashtbl.find h b == 0) bs)) (result + 1)
                in
                aux bricks (result + (aux2 [brick] 0) - 1)
        in
        aux bricks 0


let solve1 bricks =
        let s = supported_by bricks in
        Core.List.count ~f:(fun brick -> 
                let bs = List.filter (blocked_by brick) bricks in
                List.for_all (fun b -> (Hashtbl.find s b) > 1) bs
        ) bricks


let lines = In_channel.input_lines stdin

let bricks = lines
|> List.map parse_brick 
|> sortbricks
|> List.fold_left dropbrick []
|> sortbricks

let brickvs = get_vertices (fun b -> List.filter (blocked_by b) bricks) bricks
let bricks2 = topological_sort brickvs |> List.rev

let () = Printf.printf "%d\n" (solve1 bricks)
let () = Printf.printf "%d\n" (will_fall bricks2)

