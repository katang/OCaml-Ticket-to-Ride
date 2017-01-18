open Async.Std

(* A type information on a player in a Ticket to Ride game *)
type player_state = {
  pid : Player.player;
  ptype : Player.player_type;
  color : Color.color;
	train_hand : Card.TrainCard.hand;
  ticket_hand : Card.TicketCard.hand;
  trains_left : int;
  score : int
  }

(* The type of a state of a Ticket to Ride game*)
type state = {
  board: Board.board;
  train_deck : Card.TrainCard.deck;
  ticket_deck : Card.TicketCard.deck;
	player_info : player_state list
  }

(* A ref containing the current state of the Ticket to Ride game *)
val current_state : state ref

(* A ref containing an Ivar that, when filled, holds the information
 * necessary for the GUI to update the display *)
val current_gui_state :(Board.board * Player.player * Card.TicketCard.hand
* Card.TrainCard.hand * Card.TrainCard.t list *int * bool)Ivar.t ref
(* A ref containing an Ivar that, when filled, communicates the action
 * taken by a human player to the game engine. *)
val human_action: Action.action Ivar.t ref

(* checks if a player is a human player *)
val human_player : Player.player -> state -> bool

(* the hand of a player in a given state of the game *)
val player_hand : Player.player -> state -> Card.TrainCard.hand

(* the state of the game after a turn is taken *)
val do_turn : int -> state -> unit

(* the score of a player in a given state of the game *)
val score_player : Player.player -> state -> int

(* the winner of the game in a given game state *)
val determine_winner : state -> Player.player

(* runs the game *)
val main : unit