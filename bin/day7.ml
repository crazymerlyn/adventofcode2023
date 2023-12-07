open Core.Fn

type hand = {
        cards: int list;
        bid: int;
}

let card_rank c = match c with
        | '2'..'9' -> int_of_char c - int_of_char '0'
        | 'T' -> 10
        | 'J' -> 11
        | 'Q' -> 12
        | 'K' -> 13
        | 'A' -> 14
        | _ -> failwith "unrecognized"

let parse_cards string = Core.String.to_list string
  |> List.map card_rank

let parse_hand line = let parts = (String.split_on_char ' ' line) in 
{
        cards= parse_cards (List.nth parts 0);
        bid = int_of_string (List.nth parts 1);
}

let find_or_default map x z = match Hashtbl.find_opt map x with
| Some(y) -> y
| None -> z

let replace a b = List.map (fun x -> if x == a then b else x)

let most_common lst =
 let map = Hashtbl.create (List.length lst) in
 List.iter (fun x -> Hashtbl.replace map x ((find_or_default map x 0) + 1)) lst;
 let max_key, max_value = Hashtbl.fold (fun k v (k',v') -> if v' > v || (v' == v && k' > k) then (k', v') else (k, v)) map (0,0) in
 max_key


let get_counts cards = cards
  |> List.sort compare
  |> Core.List.group ~break:(<>)
  |> List.map List.length
  |> List.sort (flip Int.compare)

let get_counts2 cards = 
        match List.filter ((<>) 1) cards with
        | [] -> get_counts cards
        | filtered -> get_counts (replace 1 (most_common filtered) cards)

let compare_cards c1 c2 = 
        List.compare (List.compare compare)
        [(get_counts c1.cards); c1.cards]
        [(get_counts c2.cards); c2.cards]


let compare_cards2 c1 c2 =
        let c1 = (replace 11 1 c1.cards) in
        let c2 = (replace 11 1 c2.cards) in
        List.compare (List.compare compare)
        [(get_counts2 c1); c1]
        [(get_counts2 c2); c2]

let calculate_value cards = cards
  |> List.mapi (fun i card -> (i + 1) * card.bid)
  |> List.fold_left (+) 0

let cards = In_channel.input_lines stdin |> List.map parse_hand
let sorted_cards = List.sort compare_cards cards
let sorted_cards2 = List.sort compare_cards2 cards

let () = Printf.printf "%d\n" (calculate_value sorted_cards)
let () = Printf.printf "%d\n" (calculate_value sorted_cards2)
