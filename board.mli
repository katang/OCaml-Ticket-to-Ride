open Player
open Color

(* The types of cities and routes on a Ticket to Ride board *)
type city = {name : string; connections : string list}
type route = {c0 : city; c1 : city; color : color; owner : player; length : int}

(* the type of a Ticket to Ride board *)
type board = {cities : city list; routes : route list}

(* [new_board] is the initial board with all routes unclaimed *)
val new_board : board

(* Some exposed cities and routes *)
val vc : city
val se : city
val ca : city
val ol : city
val da : city
val ny : city
val la : city
val du : city
val hu : city
val sm : city
val na : city
val al : city
val de : city
val pi : city
val sf : city
val ci : city
val ph : city
val fe : city
val bo : city
val mi : city

val ab1 : route
val ac : route
val bc : route
val ts1 : route
val tv : route

(* [cnames_list ()] generates a string list of all city names found on the
 * board *)
val cnames_list : unit -> string list

(* [routes_between c0 c1 b] is the list of routes between the cities
 * [c0] and [c1] on the board [b] *)
val routes_between : city -> city -> board -> route list

(* [routes_between s0 s1 b] is the list of routes between the cities with
 * names [s0] and [s1] on the board [b]. *)
val routes_between_string: string -> string -> board -> route list

(* determines the shortest path needed to connect two cities on a board *)
val shortest_path : player -> city -> city -> board -> int * route list

(* [determines p c0 c1 b] is true if player [p] controls a path between cities
 * [c0] and [c1] on the board [b]. *)
val has_connected : player -> city -> city -> board -> bool

(* attempts to claim a route on a board *)
val claim_route : player -> route -> board -> bool * board

