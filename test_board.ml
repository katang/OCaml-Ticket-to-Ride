open OUnit2
open Board
open Color
open Player



let tests = [
"basic" >:: (fun _ -> assert_equal
	(1,[ab1])
		(shortest_path Player1 vc se new_board));

"alternate" >:: (fun _ -> assert_equal
	(3,[ac])
		(shortest_path Player1 vc ca new_board));

"longer" >:: (fun _ -> assert_equal
	(3,[ts1;tv])
		(shortest_path Player1 ol da new_board));

"has_connected1" >:: (fun _ -> assert_equal false
  (has_connected Player1 vc se new_board));

"claim_route1" >:: (fun _ -> assert_equal true
  (claim_route Player1 ab1 new_board |> fst));

"has_connected2" >:: (fun _ -> assert_equal true
  (claim_route Player1 ab1 new_board |> snd |> has_connected Player1 vc se));

"claim_route2" >:: (fun _ -> assert_equal false
    (claim_route Player1 ab1 new_board |> snd |> claim_route Player2 {ab1 with
     owner = Player1} |> fst));
]