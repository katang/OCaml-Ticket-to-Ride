open Card

(* determines an action to take *)
val do_turn : Board.board -> TicketCard.hand -> TrainCard.hand ->
TrainCard.t list -> Player.player -> bool -> Action.action

(* finds the best route on a board to take *)
val determine_best_route : Player.player -> (Board.city * Board.city) list ->
 Board.board -> Board.route list
