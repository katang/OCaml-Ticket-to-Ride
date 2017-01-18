open OUnit2
open Board
open Color
open Card


let tests = [

"draw_train" >:: (fun _ ->
	let d = TrainCard.new_deck in
	assert_equal (Some (List.hd d.draw_pile),{d with draw_pile=(List.tl d.draw_pile)})
		(TrainCard.draw d));


]