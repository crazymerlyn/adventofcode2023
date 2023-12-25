open Z3

type pos = float * float * float
type speed = pos
type hailstone = pos * speed

let parse_pos string =
  let parts = String.split_on_char ',' string |> List.map String.trim in
  (List.nth parts 0 |> float_of_string,
  List.nth parts 1 |> float_of_string,
  List.nth parts 2 |> float_of_string)

let parse_hailstone line = 
  let parts = String.split_on_char '@' line in
  (parse_pos (List.nth parts 0), parse_pos (List.nth parts 1))

type point = float * float 
type line = {p: point; v: point} 

let get_xyline ((x,y,_),(dx,dy,_)) = { p=(x,y);v=(dx,dy) }

let low = 2e14
let high = 4e14



let check_intersection l1 l2 =
  let (apx,apy) = l1.p in
  let (avx,avy) = l1.v in
  let (bpx,bpy) = l2.p in
  let (bvx,bvy) = l2.v in
  let ma = avy /. avx in
  let mb = bvy /. bvx in
  let ca = apy -. ma *. apx in
  let cb = bpy -. mb *. bpx in
  if Float.equal ma mb then
    None
  else
    let xpos = (cb -. ca) /. (ma -. mb) in
    let ypos = ma *. xpos +. ca in
    Some (xpos, ypos)



let stones : hailstone list = In_channel.input_lines stdin |> List.map parse_hailstone

let in_area (x,y) = low <= x && x <= high && low <= y && y <= high

let in_future {p=(x1,_);v=(vx,_)} (x2,_) = vx *. (x2 -. x1) >= 0.

let path a b = 
  let a = get_xyline a in
  let b = get_xyline b in
  let c = check_intersection a b in
  match c with
  | None -> false
  | Some(p) -> 
      in_area p && in_future a p && in_future b p

let solve stones =
  let rec aux stones result = match stones with
  | [] -> result
  | stone :: stones -> aux stones (result + Core.List.count ~f:(fun s -> path stone s) stones)
  in
  aux stones 0



let solve2 stones =
  let ctx = mk_context [] in
  let solver = Solver.mk_solver ctx None in
  let x = Expr.mk_const ctx (Symbol.mk_string ctx "x") (Arithmetic.Real.mk_sort ctx) in
  let y = Expr.mk_const ctx (Symbol.mk_string ctx "y") (Arithmetic.Real.mk_sort ctx) in
  let z = Expr.mk_const ctx (Symbol.mk_string ctx "z") (Arithmetic.Real.mk_sort ctx) in
  let vx = Expr.mk_const ctx (Symbol.mk_string ctx "vx") (Arithmetic.Real.mk_sort ctx) in
  let vy = Expr.mk_const ctx (Symbol.mk_string ctx "vy") (Arithmetic.Real.mk_sort ctx) in
  let vz = Expr.mk_const ctx (Symbol.mk_string ctx "vz") (Arithmetic.Real.mk_sort ctx) in
  let make_expr p s t = Arithmetic.mk_add ctx [p; Arithmetic.mk_mul ctx [s; t]] in
  let make_eq p1 s1 p2 s2 t = Boolean.mk_eq ctx (make_expr p1 s1 t) (make_expr p2 s2 t) in
  let () = Solver.add solver (Core.List.concat_mapi ~f:(
    fun i ((px,py,pz),(pvx,pvy,pvz)) ->
      let px = Arithmetic.Real.mk_numeral_i ctx (int_of_float px) in
      let py = Arithmetic.Real.mk_numeral_i ctx (int_of_float py) in
      let pz = Arithmetic.Real.mk_numeral_i ctx (int_of_float pz) in
      let pvx = Arithmetic.Real.mk_numeral_i ctx (int_of_float pvx) in
      let pvy = Arithmetic.Real.mk_numeral_i ctx (int_of_float pvy) in
      let pvz = Arithmetic.Real.mk_numeral_i ctx (int_of_float pvz) in
      let t = Expr.mk_const ctx (Symbol.mk_string ctx ("t" ^ string_of_int i)) (Arithmetic.Real.mk_sort ctx) in
      [
        make_eq x vx px pvx t;
        make_eq y vy py pvy t;
        make_eq z vz pz pvz t;
      ]
      ) (Core.List.take stones 3)) in
  match Solver.check solver [] with
  | SATISFIABLE -> 
      let model = Solver.get_model solver |> Option.get in
      let get_val x = (Model.get_const_interp_e model x |> Option.get |> Arithmetic.Real.get_numerator |> Arithmetic.Integer.get_big_int |> Z.to_int) in
      get_val x + get_val y + get_val z
  | _ -> failwith "Unsolvable"



let () = Printf.printf "%d\n" (solve stones)
let () = Printf.printf "%d\n" (solve2 stones)
