open Color

(* A [Card] implementation that manipulates the train cards in Ticket to Ride *)
module TrainCard = struct
	type t = color

	type hand = {rainbow:int;red:int;blue:int;yellow:int;green:int;orange:int;
		pink:int;white:int;black:int}

	type deck = {draw_pile: t list; discard_pile: t list; faceup : t list}

	let rec get_faceup (draw_pile,faceup) =
	  match draw_pile with
	  | [] -> failwith "Empty deck"
	  | h::t -> (
	    if ((List.length faceup)=5) then (draw_pile, faceup)
	    else get_faceup (t, h::faceup))


	let new_deck =
		let () = Random.self_init ()
		in
		let rec add_cards c n d =
			if n=0 then d else add_cards c (n-1) (((Random.int 400),c)::d)
		in
		let (n,l) = add_cards Rainbow 14 []
		|> add_cards Red 12
		|> add_cards Blue 12
		|> add_cards Yellow 12
		|> add_cards Green 12
		|> add_cards Orange 12
		|> add_cards Pink 12
		|> add_cards White 12
		|> add_cards Black 12
		|> List.fast_sort (fun (n0,c1) (n1,c1) -> compare n0 n1)
		|> List.split
		in
		let draw_cards = get_faceup (l,[]) in
		{draw_pile=fst draw_cards; discard_pile=[];faceup=snd draw_cards}

	let empty_hand = {rainbow=0;red=0;blue=0;yellow=0;green=0;orange=0;
		pink=0;white=0;black=0}

	let is_empty d =
		List.length d.draw_pile = 0

	let draw d =
		if (is_empty d) then (None, d)
		else (Some (List.hd d.draw_pile),
			{d with draw_pile=(List.tl d.draw_pile)})

	let discard c d =
		{d with discard_pile = c::d.discard_pile}

	let hand_contains c h n =
		match c with
		|Rainbow -> h.rainbow>=n
		|Red -> h.red>=n
		|Blue -> h.blue>=n
		|Yellow -> h.yellow>=n
		|Green -> h.green>=n
		|Orange -> h.orange>=n
		|Pink -> h.pink>=n
		|White -> h.white>=n
		|Black -> h.black>=n
		|Colorless -> false

	let hand_has c h =
	  match c with
		|Rainbow -> h.rainbow
		|Red -> h.red
		|Blue -> h.blue
		|Yellow -> h.yellow
		|Green -> h.green
		|Orange -> h.orange
		|Pink -> h.pink
		|White -> h.white
		|Black -> h.black
		|Colorless -> 0

	let add_to_hand c h =
		match c with
		|Rainbow -> {h with rainbow = h.rainbow+1}
		|Red -> {h with red = h.red+1}
		|Blue -> {h with blue = h.blue+1}
		|Yellow -> {h with yellow = h.yellow+1}
		|Green -> {h with green = h.green+1}
		|Orange -> {h with orange = h.orange+1}
		|Pink -> {h with pink = h.pink+1}
		|White -> {h with white = h.white+1}
		|Black -> {h with black = h.black+1}
		|Colorless -> h

	let remove_from_hand c h n =
		match c with
		|Rainbow -> {h with rainbow = h.rainbow-n}
		|Red -> {h with red = h.red-n}
		|Blue -> {h with blue = h.blue-n}
		|Yellow -> {h with yellow = h.yellow-n}
		|Green -> {h with green = h.green-n}
		|Orange -> {h with orange = h.orange-n}
		|Pink -> {h with pink = h.pink-n}
		|White -> {h with white = h.white-n}
		|Black -> {h with black = h.black-n}
		|Colorless -> h

  (* shuffles a deck of cards *)
	let shuffle d =
		let () = Random.self_init ()
		in
		let (n,l) = List.rev_map (fun c -> (Random.int 400),c) d.discard_pile
			|> List.fast_sort (fun (n0,c0) (n1,c1) -> compare n0 n1)
			|> List.split
		in
		let draw_cards = get_faceup (l,[]) in
		{draw_pile=fst draw_cards; discard_pile = []; faceup=snd draw_cards}

	let difference a b =
		let x = {rainbow=0;red=a.red-b.red;blue=a.blue-b.blue;yellow=a.yellow-b.yellow;
		green=a.green-b.green;orange=a.orange-b.orange;pink=a.pink-b.pink;
		white=a.white-b.white;black=a.black-b.black} in
		{x with rainbow=a.rainbow-(x.red+x.blue+x.yellow+x.green+x.orange+x.pink+x.white+x.black)}

end

(* A [Card] implementation that manipulates the ticket cards in Ticket to Ride *)
module TicketCard = struct
	type t = {c0 : Board.city; c1 : Board.city; points : int}
	type hand = t list

	type deck = {draw_pile : t list; discard_pile : t list}

	let to_pair t = (t.c0,t.c1)

	let to_list h = List.map to_pair h

	(* All ticket cards *)
	let la_ny = {c0=Board.la;c1=Board.ny;points=21}
	let du_hu = {c0=Board.du;c1=Board.hu;points=8}
	let sm_na = {c0=Board.sm;c1=Board.na;points=8}
	let ny_al = {c0=Board.ny;c1=Board.al;points=6}
	let vc_na = {c0=Board.vc;c1=Board.na;points=17}
	let ca_ol = {c0=Board.ca;c1=Board.ol;points=10}
	let ny_hu = {c0=Board.ny;c1=Board.hu;points=5}
	let de_pi = {c0=Board.de;c1=Board.pi;points=11}
	let sf_al = {c0=Board.sf;c1=Board.al;points=17}
	let la_ci = {c0=Board.la;c1=Board.ci;points=16}
	let ca_ph = {c0=Board.ca;c1=Board.ph;points=13}
	let vc_fe = {c0=Board.vc;c1=Board.fe;points=13}
	let bo_mi = {c0=Board.bo;c1=Board.mi;points=12}
	let la_mi = {c0=Board.la;c1=Board.mi;points=20}
	let ci_sf = {c0=Board.ci;c1=Board.sf;points=9}

	let new_deck = let dp = [la_ny;du_hu;sm_na;ny_al;vc_na;ca_ol;ny_hu;de_pi;
	  sf_al;la_ci;ca_ph;vc_fe;bo_mi;la_mi;ci_sf]
		in
		{draw_pile=dp;discard_pile=[]}

	let empty_hand = []

	let is_empty d = List.length d.draw_pile = 0

	let draw d = if is_empty d then (None,d)
		else (Some (List.hd d.draw_pile),
			{d with draw_pile=(List.tl d.draw_pile)})

	let discard c d = {d with draw_pile=(c::d.discard_pile)}

	let hand_contains c h = List.mem c h

	let add_to_hand c h = c::h

	let remove_from_hand c h =
		let rec rem  e l = match l with
		|[] -> []
		|h::t when h=e -> t
		|h::t -> rem e t |> List.cons h (* not tail recursive, but only used on small lists *)
		in
		rem c h

	let to_pair c =  c.c0,c.c1

end
