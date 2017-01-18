open Action
open GMain
open GdkKeysyms
open Board
open Card
open Async.Std

let locale = GtkMain.Main.init ()

let filler () = ["a";"b";"c"]
(* Global refences to drop down objects *)
let select_c0 = ref (GEdit.combo ~popdown_strings:(filler ()) ())
let select_c1 = ref (GEdit.combo ~popdown_strings:(filler ()) ())
let colors = GEdit.combo
  ~popdown_strings: ["red";"blue";"yellow";"green";"orange";"pink";"white";
  "black"; "rainbow"] ~width:150 ()

(* Global references for deck card objects*)
let deck = ref (GPack.table ())
let claims = ref (GPack.table ())
let button0 = ref (GButton.button ())
let button1 = ref (GButton.button ())
let button2 = ref (GButton.button ())
let button3 = ref (GButton.button ())
let button4 = ref (GButton.button ())
let button5 = ref (GButton.button ())

(* Global references for hand card objects *)
let hand_row = ref (GPack.table ())
let rainbow_box = ref (GPack.hbox ())
let red_box = ref (GPack.hbox ())
let blue_box = ref (GPack.hbox ())
let yellow_box = ref (GPack.hbox ())
let green_box = ref (GPack.hbox ())
let orange_box = ref (GPack.hbox ())
let pink_box = ref (GPack.hbox ())
let white_box = ref (GPack.hbox ())
let black_box = ref (GPack.hbox ())
let rainbow_label = ref (GMisc.label ())
let red_label = ref (GMisc.label ())
let blue_label = ref (GMisc.label ())
let yellow_label = ref (GMisc.label ())
let green_label = ref (GMisc.label ())
let orange_label = ref (GMisc.label ())
let pink_label = ref (GMisc.label ())
let white_label = ref (GMisc.label ())
let black_label = ref (GMisc.label ())

(* Global references to train and player tracker objects *)
let train_button = ref (GButton.button ~label:"70 trains" ())
let player_button = ref (GButton.button ~label:"Current player: Player 1" ())

let window = ref (GWindow.window ~width: 1600 ~height: 850
                              ~title: "Ticket to Ride"
                              ~resizable: false ())

(* Global references to check button clicking *)
let draw0 = ref (false)
let draw1 = ref (false)
let draw2 = ref (false)
let draw3 = ref (false)
let draw4 = ref (false)
let draw_deck = ref (false)
let claim_route = ref (false)
let req_tickets = ref (false)

(* match [color] with correct card image *)
let match_jpg color =
  match color with
  | Color.Rainbow -> "images/rainbow_card.jpg"
  | Color.Red -> "images/red_card.jpg"
  | Color.Blue -> "images/blue_card.jpg"
  | Color.Yellow -> "images/yellow_card.jpg"
  | Color.Green -> "images/green_card.jpg"
  | Color.Orange -> "images/orange_card.jpg"
  | Color.Pink -> "images/purple_card.jpg"
  | Color.White -> "images/white_card.jpg"
  | Color.Black -> "images/black_card.jpg"
  | _ -> failwith "Not a color"

(* convert player pid [p] to player string *)
let get_player p =
  match p with
  | Player.None -> "No Player"
  | Player.Player1 -> "Player 1"
  | Player.Player2 -> "Player 2"
  | Player.Player3 -> "Player 3"
  | Player.Player4 -> "Player 4"
  | Player.Player5 -> "Player 5"

(* return matching string of color *)
let color_str color =
  match color with
  | Color.Rainbow -> "rainbow"
  | Color.Red -> "red"
  | Color.Blue -> "blue"
  | Color.Yellow -> "yellow"
  | Color.Green -> "green"
  | Color.Orange -> "orange"
  | Color.Pink -> "pink"
  | Color.White -> "white"
  | Color.Black -> "black"
  | Color.Colorless -> "colorless"
  | _ -> failwith "Not a color"

(* return string list of player's routes [proutes] with routes in the format
 * c0 name-c1 name-color *)
let rec str_routes proutes =
  match proutes with
  | [] -> []
  | h::t -> (
    let sr = h.c0.name^"-"^h.c1.name^"-"^(color_str h.color) in
    sr::(str_routes t))

(* matches color string and returns corresponding type Color *)
let get_color color_str =
  match color_str with
  | "colorless" -> Color.Colorless
  | "rainbow" -> Color.Rainbow
  | "red" -> Color.Red
  | "blue" -> Color.Blue
  | "yellow" -> Color.Yellow
  | "green" -> Color.Green
  | "orange" -> Color.Orange
  | "pink" -> Color.Pink
  | "white" -> Color.White
  | "black" -> Color.Black
  | _ -> failwith "Not a color"

(* returns routes between city strings [s0] and [s1] on board [b] with
 * matching color [color] in a list *)
let get_colorroutes s0 s1 b color =
  let all_routes = Board.routes_between_string s0 s1 b in
  List.filter (fun x -> (x.color=color || x.color=Color.Colorless)
    && x.owner=Player.None) all_routes

(* updates gui buttons and labels based on human action to reflect game state *)
let rec update_buttons state action board p train_hand trains =
  let _ = if !draw0 = true then
            (draw0 := false;
            Ivar.fill (!action) (DrawFaceUp 0))
          else if !draw1 = true then
            (draw1 := false;
            Ivar.fill !action (DrawFaceUp 1))
          else if !draw2 = true then
            (draw2 := false;
            Ivar.fill !action (DrawFaceUp 2))
          else if !draw3 = true then
            (draw3 := false;
            Ivar.fill !action (DrawFaceUp 3))
          else if !draw4 = true then
            (draw4 := false;
            Ivar.fill !action (DrawFaceUp 4))
          else if !draw_deck = true then
            (draw_deck := false;
            Ivar.fill !action (DrawDeck))
          else
            (claim_route := false;
            let card_color = get_color (colors#entry#text) in
            let chosen_route = get_colorroutes (!select_c0#entry#text) (!select_c1#entry#text) board card_color in
            if (chosen_route <> []) then Ivar.fill !action (ClaimRoute ((List.hd chosen_route), card_color))
            else
              (let dum_city = {name="no";connections=[]} in
              let dum_route = {c0=dum_city;c1=dum_city;color=card_color;owner=Player.None;length=1} in
              Ivar.fill !action (ClaimRoute (dum_route, card_color)))) in
  upon (Ivar.read !state) (fun (board, p, ticket_hand, train_hand, face_ups, trains, rainbow) ->

  (* update face up card buttons *)
  !button0#destroy (); !button1#destroy (); !button2#destroy ();
  !button3#destroy (); !button4#destroy ();
   button0 := GButton.button ();
  !deck#attach ~left:0 ~top:0 (!button0#coerce);
  ignore(!button0#connect#clicked ~callback:(fun () -> draw0 := true;
    update_buttons state action board p train_hand trains));
  button1 := GButton.button ();
  !deck#attach  ~left:0 ~top:1 (!button1#coerce);
  ignore(!button1#connect#clicked ~callback:(fun () -> draw1 := true;
    update_buttons state action board p train_hand trains));
  button2 := GButton.button ();
  !deck#attach  ~left:0 ~top:2 (!button2#coerce);
  ignore(!button2#connect#clicked ~callback:(fun () -> draw2 := true;
    update_buttons state action board p train_hand trains));
  button3 := GButton.button ();
  !deck#attach  ~left:0 ~top:3 (!button3#coerce);
  ignore(!button3#connect#clicked ~callback:(fun () -> draw3 := true;
    update_buttons state action board p train_hand trains));
  button4 := GButton.button ();
  !deck#attach  ~left:0 ~top:4 (!button4#coerce);
  ignore(!button4#connect#clicked ~callback:(fun () -> draw4 := true;
    update_buttons state action board p train_hand trains));

  let i1 = match_jpg (List.nth face_ups 0) in
  let _img1 = GMisc.image ~file:i1
  ~packing: !button0#add () in
  let i2 = match_jpg (List.nth face_ups 1) in
  let _img2 = GMisc.image ~file:i2
  ~packing: !button1#add () in
  let i3 = match_jpg (List.nth face_ups 2) in
  let _img3 = GMisc.image ~file:i3
  ~packing: !button2#add () in
  let i4 = match_jpg (List.nth face_ups 3) in
  let _img4 = GMisc.image ~file:i4
  ~packing: !button3#add () in
  let i5 = match_jpg (List.nth face_ups 4) in
  let _img5 = GMisc.image ~file:i5
  ~packing: !button4#add () in

  (* Update counter *)
  let rainbow_num = string_of_int(TrainCard.hand_has Color.Rainbow train_hand) in
  let red_num = string_of_int(TrainCard.hand_has Color.Red train_hand) in
  let blue_num = string_of_int(TrainCard.hand_has Color.Blue train_hand) in
  let yellow_num = string_of_int(TrainCard.hand_has Color.Yellow train_hand) in
  let green_num = string_of_int(TrainCard.hand_has Color.Green train_hand) in
  let orange_num = string_of_int(TrainCard.hand_has Color.Orange train_hand) in
  let pink_num = string_of_int(TrainCard.hand_has Color.Pink train_hand) in
  let white_num = string_of_int(TrainCard.hand_has Color.White train_hand) in
  let black_num = string_of_int(TrainCard.hand_has Color.Black train_hand) in

  !rainbow_label#destroy (); !red_label#destroy (); !blue_label#destroy ();
  !yellow_label#destroy (); !green_label#destroy (); !orange_label#destroy ();
  !pink_label#destroy (); !white_label#destroy (); !black_label#destroy ();
  let rainbow_markup = "<span foreground=\"black\" size = \
  \"x-large\">"^rainbow_num^"</span>" in
  rainbow_label := GMisc.label ~markup:rainbow_markup ~packing:!rainbow_box#pack ();
  let red_markup = "<span foreground=\"black\" size = \
  \"x-large\">"^red_num^"</span>" in
  red_label := GMisc.label ~markup:red_markup ~packing:!red_box#pack ();
  let blue_markup = "<span foreground=\"black\" size = \
  \"x-large\">"^blue_num^"</span>" in
  blue_label := GMisc.label ~markup:blue_markup ~packing:!blue_box#pack ();
  let yellow_markup = "<span foreground=\"black\" size = \
  \"x-large\">"^yellow_num^"</span>" in
  yellow_label := GMisc.label ~markup:yellow_markup ~packing:!yellow_box#pack ();
  let green_markup = "<span foreground=\"black\" size = \
  \"x-large\">"^green_num^"</span>" in
  green_label := GMisc.label ~markup:green_markup ~packing:!green_box#pack ();
  let orange_markup = "<span foreground=\"black\" size = \
  \"x-large\">"^orange_num^"</span>" in
  orange_label := GMisc.label ~markup:orange_markup ~packing:!orange_box#pack ();
  let pink_markup = "<span foreground=\"black\" size = \
  \"x-large\">"^pink_num^"</span>" in
  pink_label := GMisc.label ~markup:pink_markup ~packing:!pink_box#pack ();
  let white_markup = "<span foreground=\"black\" size = \
  \"x-large\">"^white_num^"</span>" in
  white_label := GMisc.label ~markup:white_markup ~packing:!white_box#pack ();
  let black_markup = "<span foreground=\"black\" size = \
  \"x-large\">"^black_num^"</span>" in
  black_label := GMisc.label ~markup:black_markup ~packing:!black_box#pack ();

  (* update current player label *)
  !player_button#destroy ();
  !train_button#destroy ();
  let curr_player = "Current player: "^(get_player p) in
  player_button := GButton.button ~label:curr_player ();
  !claims#attach ~left:0 ~top:4 (!player_button#coerce);
  let trains_left = string_of_int(trains)^" trains" in
  train_button := GButton.button ~label:trains_left ();
  !claims#attach ~left:0 ~top:3 (!train_button#coerce);

  (* Print players and the routes they own *)
  printf "\nCurrent routes owned by %s: \n" (get_player p);
  let psr = str_routes (List.filter (fun x -> x.owner=p) board.routes) in
  List.iter (printf "%s, ") psr;

  state := Ivar.create ();)

(* the main gui *)
let main_gui state action () =
  upon (Ivar.read !state) (fun (board, p, ticket_hand, train_hand, face_ups, trains, rainbow) ->
  window := GWindow.window ~width: 1600 ~height: 850
                              ~title: "Ticket to Ride"
                              ~resizable: false ();
  let bigbox = GPack.hbox ~packing:!window#add ~border_width:5 () in
  deck := GPack.table ~rows:6 ~columns:1 ~homogeneous:true
    ~packing:(bigbox#pack ~from: `END) ();
  claims := GPack.table ~rows:3 ~columns:1 ~homogeneous:false
  ~packing:(bigbox#pack ~from: `END) ();
  let bigtable = GPack.table ~rows:2 ~columns:1 ~width: 900 ~homogeneous:false
  ~packing:bigbox#pack () in
  let fixed = GPack.fixed () in
  let bg_img = GMisc.image ~file: "images/board1.jpg" () in
  fixed#put bg_img#coerce ~x:105 ~y:0;
  bigtable#attach ~left: 0 ~top:0 (fixed#coerce);
  let claims_table = GPack.table ~rows:1 ~columns:3  ~width:450
  ~homogeneous:true () in
  let claim_button = GButton.button ~label: "Claim" () in
  ignore(claim_button#connect#clicked ~callback: (fun () -> claim_route := true;
  update_buttons state action board p train_hand trains));
  !claims#attach ~left: 0 ~top: 0 (claims_table#coerce);
  !claims#attach ~left: 0 ~top: 1 (claim_button#coerce);
  select_c0 := GEdit.combo ~popdown_strings:(Board.cnames_list ())
    ~case_sensitive:false ~allow_empty:false ~width: 150 ();
  select_c1 := GEdit.combo ~popdown_strings:(Board.cnames_list ())
    ~case_sensitive:false ~allow_empty:false ~width: 150 ();
  claims_table#attach !select_c0#coerce ~left:0 ~top:0;
  claims_table#attach !select_c1#coerce ~left:1 ~top:0;
  claims_table#attach colors#coerce ~left:2 ~top:0;

  (* current player and number of trains they have left *)
  let trains_left = string_of_int(trains)^" trains" in
  train_button := GButton.button ~label:trains_left ();
  let curr_player = "Current player: "^(get_player p) in
  player_button := GButton.button ~label:curr_player ();
  !claims#attach ~left:0 ~top:3 (!train_button#coerce);
  !claims#attach ~left:0 ~top:4 (!player_button#coerce);

  (* face up card buttons *)
  button0 := GButton.button ();
  !deck#attach ~left:0 ~top:0 (!button0#coerce);
  ignore(!button0#connect#clicked ~callback:(fun () -> draw0 := true;
  update_buttons state action board p train_hand trains));
  button1 := GButton.button ();
  !deck#attach  ~left:0 ~top:1 (!button1#coerce);
  ignore(!button1#connect#clicked ~callback:(fun () -> draw1 := true;
    update_buttons state action board p train_hand trains));
  button2 := GButton.button ();
  !deck#attach  ~left:0 ~top:2 (!button2#coerce);
  ignore(!button2#connect#clicked ~callback:(fun () -> draw2 := true;
    update_buttons state action board p train_hand trains));
  button3 := GButton.button ();
  !deck#attach  ~left:0 ~top:3 (!button3#coerce);
  ignore(!button3#connect#clicked ~callback:(fun () -> draw3 := true;
    update_buttons state action board p train_hand trains));
  button4 := GButton.button ();
  !deck#attach  ~left:0 ~top:4 (!button4#coerce);
  ignore(!button4#connect#clicked ~callback:(fun () -> draw4 := true;
    update_buttons state action board p train_hand trains));
  button5 := GButton.button ();
  !deck#attach  ~left:0 ~top:5 (!button5#coerce);
  ignore(!button5#connect#clicked ~callback:(fun () -> draw_deck := true;
    update_buttons state action board p train_hand trains));

  (* Images button labels for face up cards and deck *)
  let i1 = match_jpg (List.nth face_ups 0) in
  let _img1 = GMisc.image ~file:i1
  ~packing: !button0#add () in
  let i2 = match_jpg (List.nth face_ups 1) in
  let _img2 = GMisc.image ~file:i2
  ~packing: !button1#add () in
  let i3 = match_jpg (List.nth face_ups 2) in
  let _img3 = GMisc.image ~file:i3
  ~packing: !button2#add () in
  let i4 = match_jpg (List.nth face_ups 3) in
  let _img4 = GMisc.image ~file:i4
  ~packing: !button3#add () in
  let i5 = match_jpg (List.nth face_ups 4) in
  let _img5 = GMisc.image ~file:i5
  ~packing: !button4#add () in
  let _img6 = GMisc.image ~file: "images/deck_card.jpg"
  ~packing: !button5#add () in

  hand_row := GPack.table ~rows:3 ~columns:6 ~homogeneous:true ();
  bigtable#attach ~left:0 ~top:1 (!hand_row#coerce);

  (* Card hand counter *)
  let rainbow_num = string_of_int(TrainCard.hand_has Color.Rainbow train_hand) in
  let red_num = string_of_int(TrainCard.hand_has Color.Red train_hand) in
  let blue_num = string_of_int(TrainCard.hand_has Color.Blue train_hand) in
  let yellow_num = string_of_int(TrainCard.hand_has Color.Yellow train_hand) in
  let green_num = string_of_int(TrainCard.hand_has Color.Green train_hand) in
  let orange_num = string_of_int(TrainCard.hand_has Color.Orange train_hand) in
  let pink_num = string_of_int(TrainCard.hand_has Color.Pink train_hand) in
  let white_num = string_of_int(TrainCard.hand_has Color.White train_hand) in
  let black_num = string_of_int(TrainCard.hand_has Color.Black train_hand) in

  (* Labels for hand cards*)
  let _img6 = GMisc.image ~file:"images/rainbow_card.jpg"
   ~packing:!rainbow_box#pack () in
  let _img6 = GMisc.image ~file:"images/red_card.jpg"
   ~packing:!red_box#pack () in
  let _img6 = GMisc.image ~file:"images/blue_card.jpg"
  ~packing:!blue_box#pack () in
  let _img6 = GMisc.image ~file:"images/yellow_card.jpg"
   ~packing:!yellow_box#pack () in
  let _img6 = GMisc.image ~file:"images/green_card.jpg"
   ~packing:!green_box#pack () in
  let _img6 = GMisc.image ~file:"images/orange_card.jpg"
  ~packing:!orange_box#pack () in
  let _img6 = GMisc.image ~file:"images/purple_card.jpg"
   ~packing:!pink_box#pack () in
  let _img6 = GMisc.image ~file:"images/white_card.jpg"
  ~packing:!white_box#pack () in
  let _img6 = GMisc.image ~file:"images/black_card.jpg"
   ~packing:!black_box#pack () in

  (* Display hand card counter *)
  let rainbow_markup = "<span foreground=\"black\" size = \
  \"x-large\">"^rainbow_num^"</span>" in
  rainbow_label := GMisc.label ~markup:rainbow_markup ~packing:!rainbow_box#pack ();
  !hand_row#attach ~left: 1 ~top:1 (!rainbow_box#coerce);
  let red_markup = "<span foreground=\"black\" size = \
  \"x-large\">"^red_num^"</span>" in
  red_label := GMisc.label ~markup:red_markup ~packing:!red_box#pack ();
  !hand_row#attach ~left: 2 ~top:1 (!red_box#coerce);
  let blue_markup = "<span foreground=\"black\" size = \
  \"x-large\">"^blue_num^"</span>" in
  blue_label := GMisc.label ~markup:blue_markup ~packing:!blue_box#pack ();
  !hand_row#attach ~left: 3 ~top:1 (!blue_box#coerce);
  let yellow_markup = "<span foreground=\"black\" size = \
  \"x-large\">"^yellow_num^"</span>" in
  yellow_label := GMisc.label ~markup:yellow_markup ~packing:!yellow_box#pack ();
  !hand_row#attach ~left: 4 ~top:1 (!yellow_box#coerce);
  let green_markup = "<span foreground=\"black\" size = \
  \"x-large\">"^green_num^"</span>" in
  green_label := GMisc.label ~markup:green_markup ~packing:!green_box#pack ();
  !hand_row#attach ~left: 1 ~top:2 (!green_box#coerce);
  let orange_markup = "<span foreground=\"black\" size = \
  \"x-large\">"^orange_num^"</span>" in
  orange_label := GMisc.label ~markup:orange_markup ~packing:!orange_box#pack ();
  !hand_row#attach ~left: 2 ~top:2 (!orange_box#coerce);
  let pink_markup = "<span foreground=\"black\" size = \
  \"x-large\">"^pink_num^"</span>" in
  pink_label := GMisc.label ~markup:pink_markup ~packing:!pink_box#pack ();
  !hand_row#attach ~left: 3 ~top:2 (!pink_box#coerce);
  let white_markup = "<span foreground=\"black\" size = \
  \"x-large\">"^white_num^"</span>" in
  white_label := GMisc.label ~markup:white_markup ~packing:!white_box#pack ();
  !hand_row#attach ~left: 4 ~top:2 (!white_box#coerce);
  let black_markup = "<span foreground=\"black\" size = \
  \"x-large\">"^black_num^"</span>" in
  black_label := GMisc.label ~markup:black_markup ~packing:!black_box#pack ();
  !hand_row#attach ~left: 5 ~top:2 (!black_box#coerce);

  ignore(!window#connect#destroy ~callback: (fun () -> GMain.Main.quit ();
  Pervasives.exit 0));

  !window#show ();
  let _ = GtkThread.start () in ())
