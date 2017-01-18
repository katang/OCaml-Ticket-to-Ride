open Action
open Board
open Card
open Card.TrainCard
open Card.TicketCard
open Color

let rec remove_dups l = match l with
	|[] -> []
	|h::t -> h::(remove_dups (List.filter (fun x -> x<>h) t))

let determine_best_route p rl b =
	let (seperatel,seperater) =
		(List.map (fun (c0,c1) -> shortest_path p c0 c1 b) rl) |>
List.fold_left (fun (tot,rtot) (l,rs) -> (tot+l,(rs @ rtot))) (0,[])
	in
	let routes_to_hub p b h c0 c1 =
		let (c0d,c0r) = (shortest_path p h c0 b) in let (c1d,c1r) = (shortest_path p h c1 b) in
		(c0d+c1d,c0r @ c1r) in
	let (hubl,hubr) =
		let hub_list = List.map (fun c -> (List.map (fun (c0,c1) -> routes_to_hub p b c c0 c1)) rl
|> List.fold_left (fun (it,rst) (i,rs) -> (i+it,rs @ rst)) (0,[])) (b.cities) in
		let rec hub_min (ml,mr) = function
			|[] -> (ml,mr)
			|(hl,hr)::t -> if hl<ml then hub_min (hl,hr) t else hub_min (ml,mr) t in
		hub_min (5000000,[]) hub_list in
if (seperatel < hubl) then (seperater |> remove_dups) else (hubr |> remove_dups)

let do_turn b tkh (trh:TrainCard.hand) fup p ad =
	let dest_cities = List.fold_left (fun l tk -> match tk with (c0,c1) -> (c0,c1)::l) [] (TicketCard.to_list tkh) |> remove_dups in
	let routes_to_complete = determine_best_route p dest_cities b in
	if routes_to_complete = [] then DrawDeck else
	(let curr_route = List.hd routes_to_complete in
	let completable = match curr_route.color with
	|Red -> (trh.red >= curr_route.length, Red)
	|Blue -> (trh.blue >= curr_route.length,Blue)
	|Yellow -> (trh.yellow >= curr_route.length, Yellow)
	|Green -> (trh.green >= curr_route.length, Green)
	|Orange -> (trh.orange >= curr_route.length, Orange)
	|Pink -> (trh.pink >= curr_route.length, Pink)
	|White -> (trh.white >= curr_route.length, White)
	|Black -> (trh.black >= curr_route.length, Black)
	|Colorless ->
		let comp (c0,n0) (c1,n1) = if n0>=curr_route.length then (if n1<curr_route.length then (c0,n0)
else if n0<=n1 then (c0,n0) else (c1,n1)) else if n1>=curr_route.length then (c1,n1)
				else if n0>n1 then (c0,n0) else (c1,n1) in
		let (col,quant) = comp (Red,trh.red) (Blue,trh.blue) |> comp (Yellow,trh.yellow)
			|> comp (Green,trh.green) |> comp (Orange,trh.orange) |> comp (Pink,trh.pink)
			|> comp (White,trh.white) |> comp (Black,trh.black) in
		(quant >= curr_route.length, col)

	|Rainbow -> failwith "Rainbow routes don't exist, silly."
	in
	match completable with
	|true,c -> (ClaimRoute(curr_route,c))
	|false,c -> (
let rec cind c n l = match l with |[] -> -1 |h::t -> if c=h then n else (cind c (n+1) t) in
		match cind c 0 fup with
		| -1 -> (match cind Rainbow 0 fup with |n when n > -1 && ad=false -> DrawFaceUp(n) |_ -> DrawDeck)
		| n -> DrawFaceUp(n)
	))