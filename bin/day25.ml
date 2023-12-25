type vertex = string
type edge = vertex * vertex
module VertexSet = Set.Make(
  struct
    let compare = compare
    type t = vertex
  end)
type graph = { vertices: VertexSet.t; edges: edge list }

let edge_equal (a, b) (x, y) = (a = x && b = y) || (a = y && b = x)

let contract_random_edge graph =
  let ix = Random.int (List.length graph.edges) in
  let edge_to_remove = List.nth graph.edges ix in
  let a, b = edge_to_remove in
  let new_vertex = Printf.sprintf "%s/%s" a b in
  let vertices' =
    graph.vertices
    |> VertexSet.remove a
    |> VertexSet.remove b
    |> VertexSet.add new_vertex
  in
  let edges' =
    graph.edges
    |> List.filter (fun edge -> not (edge_equal edge edge_to_remove))
    |> List.rev_map (fun (x, y) ->
      let x' = if x = a || x = b then new_vertex else x in
      let y' = if y = a || y = b then new_vertex else y in
      x', y')
  in
  { vertices = vertices'; edges = edges' }

let rec karger graph =
  if VertexSet.cardinal graph.vertices > 2 && graph.edges <> [] then
    karger (contract_random_edge graph)
  else
    graph

let rec run_karger graph =
  let cut = karger graph in
  if List.length cut.edges <> 3 then
    run_karger graph
  else
    cut.vertices

let parse_edges line =
  let parts = String.split_on_char ':' line |> List.map String.trim in
  let children = String.split_on_char ' ' (List.nth parts 1) in
  List.map (fun c -> ((List.hd parts), c)) children

let edges = In_channel.input_lines stdin |> List.concat_map parse_edges
let vertices = (List.map fst edges @ List.map snd edges) |> VertexSet.of_list
let graph = {edges;vertices}

let () = Random.self_init ()

let solve graph = 
  let result = run_karger graph |> VertexSet.to_list in
  let size node = Core.String.count ~f:((=) '/') node + 1 in
  let a = List.hd result in
  let b = List.nth result 1 in
  size a * size b

let () = Printf.printf "%d\n" (solve graph)

