open Async.Std

(* [main_gui current_gui_state human_action ()] is the main GUI
 * of the game. It is the only value in the Gui module that needs to be exposed
 * because it allows for the Game module to initialize the GUI. It takes
 * Ivar refs as arguments because the filling of the Ivars serves as the
 * means of communication between the GUI and the game logic. *)
val main_gui: (Board.board * Player.player * Card.TicketCard.hand *
  Card.TrainCard.hand * Card.TrainCard.t list * int * bool)
  Ivar.t ref -> Action.action Ivar.t ref -> unit -> unit