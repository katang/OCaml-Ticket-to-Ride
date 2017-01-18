open OUnit2
open Board
open Color
open Player

let p1 = {player = Player1; human = true; hand = []}
let p2 = {player = Player2; human = true; hand = []}
let p3 = {player = Player3; human = true; hand = []}
let p4 = {player = Player4; human = true; hand = []}
let p5 = {player = Player5; human = false; hand = []}


let s1 = {

}

let tests = [

"basic" >:: (fun _ -> assert_equal
	(true)
		(human_player Player1 a b new_board));


]