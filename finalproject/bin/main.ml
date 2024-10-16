(* @author Narayan Rueppel (nr357), @author Aryan Agarwal (aaa344), @author Arya
   Bhandari (anb78)*)

open Finalproject
open Bogue
open Utils
open Tsdl
module W = Widget
module L = Layout
module E = Event

(* AF: The states [Starting],[Playing], and [Ending] represent the 3 phases of
   the game. The starting phase of placing troops in claimed countries, the
   gameplay phase, and the wrapping up/ending phase*)
(*RI: The game state can only progress from [Starting] to [Playing], and
  [Playing] to [Ending], at which point the game ends*)
type states =
  | Starting
  | Playing
  | Ending

(*[game_state] is the current game_state*)
let game_state = ref Starting

(*[starting_troops] is the number of troops each player starts with*)
let starting_troops = Player.soldiers.(Player.num_players - 2)

(*[countries] is the list of country names*)
let countries = Player.countries

let end_game () =
  let () = raise Bogue.Exit in
  ()

let () = Map.make ()

(*[bg] is the background color (white)*)
let bg = (255, 255, 255, 255)

(*[layouts country] returns the coordinates [(x,y)] in the gui/map associated
  with [country]*)
let layouts = function
  | "Alaska" -> (56, 105)
  | "Northwest America" -> (115, 105)
  | "Western America" -> (110, 230)
  | "Eastern America" -> (215, 260)
  | "Central America" -> (50, 294)
  | "Alberta" -> (135, 160)
  | "Ontario" -> (196, 170)
  | "Quebec" -> (265, 165)
  | "Greenland" -> (320, 60)
  | "Iceland" -> (410, 110)
  | "Great Britain" -> (320, 240)
  | "Scandinavia" -> (475, 115)
  | "Ukraine" -> (550, 180)
  | "Northern Europe" -> (445, 225)
  | "Southern Europe" -> (475, 280)
  | "Western Europe" -> (310, 310)
  | "Middle East" -> (590, 320)
  | "Afghanistan" -> (650, 240)
  | "Ural" -> (665, 150)
  | "Siberia" -> (705, 100)
  | "Yakutsk" -> (788, 80)
  | "Kamchatka" -> (860, 90)
  | "Irkutsk" -> (765, 180)
  | "Mongolia" -> (780, 230)
  | "Japan" -> (925, 220)
  | "India" -> (710, 340)
  | "China" -> (750, 290)
  | "Siam" -> (785, 370)
  | "Venezuela" -> (215, 367)
  | "Peru" -> (240, 460)
  | "Brazil" -> (260, 420)
  | "Argentina" -> (240, 520)
  | "North Africa" -> (430, 410)
  | "Egypt" -> (520, 380)
  | "East Africa" -> (560, 455)
  | "South Africa" -> (520, 575)
  | "Congo" -> (515, 500)
  | "Madagascar" -> (620, 625)
  | "Indonesia" -> (760, 440)
  | "Papua New Guinea" -> (900, 425)
  | "Western Australia" -> (820, 575)
  | "Eastern Australia" -> (900, 550)
  | _ -> (0, 0)

(*[player_color player] returns [(color,x,y)] where [color] is the color
  associated with player, and (x,y) are the position of the color label in the
  gui.*)
let player_color = function
  | 0 -> (Draw.set_alpha 255 Draw.blue, 50, 425)
  | 1 -> (Draw.set_alpha 255 Draw.black, 50, 450)
  | 2 -> (Draw.set_alpha 255 Draw.dark_grey, 50, 475)
  | 3 -> (Draw.set_alpha 255 Draw.green, 50, 500)
  | 4 -> (Draw.set_alpha 255 Draw.magenta, 50, 525)
  | _ -> (Draw.set_alpha 255 Draw.sienna, 50, 550)

(*make_label country creates a label for [country] containing the name of
  [country] and the number of troops in [country]*)
let make_label country =
  let xpos, ypos = layouts country in
  let c = List.assoc country Player.worldMap in
  match c with
  | { player = Some x; troops = Some y; name = n; _ } ->
      let color, _, _ = player_color x in
      let text = W.label ~size:10 ~fg:color (n ^ " , " ^ string_of_int y) in
      let label = L.resident ~x:xpos ~y:ypos text in
      label
  | _ -> failwith ""

(*[make_label countries] creates a label for every [country] in [countries]*)
let rec make_labels = function
  | [] -> []
  | h :: t -> make_label h :: make_labels t

(*[color_labels ()] creates labels showing the player's colors*)
let color_labels () =
  let labels = ref [] in
  let () =
    for i = 0 to 5 do
      let color, x, y = player_color i in
      let text =
        W.label ~size:12 ~fg:color ("Player " ^ string_of_int (i + 1))
      in
      let label = L.resident ~x ~y text in
      labels := label :: !labels
    done
  in
  !labels

(*[check_end_condition] checks if the game should transition to the ending
  state*)
let check_end_condition () =
  if
    (not
       (List.exists Turn.player_has_territories
          (List.init Player.num_players (Fun.id : int -> int))))
    || read_line () = "end game"
  then game_state := Ending

(*[summarize_game] prints the final state of the game including the winner*)
let summarize_game () =
  let summaries =
    List.init Player.num_players (fun i ->
        let terrs = Turn.owned_by i in
        let num_territories =
          Array.fold_left
            (fun acc opt -> if Option.is_some opt then acc + 1 else acc)
            0 terrs
        in
        (i + 1, num_territories))
  in
  let find_max summaries =
    List.fold_left
      (fun (max_player, max_territories) (player, territories) ->
        if territories > max_territories then (player, territories)
        else (max_player, max_territories))
      (0, 0) summaries
  in
  let winner = find_max summaries in
  List.iter
    (fun (player, territories) ->
      Printf.printf "Player %d: %d territories\n" player territories)
    summaries;
  Printf.printf "Player %d wins with %d territories!\n" (fst winner)
    (snd winner)

(*[refresh layout] recalculates all the labels and updates them in [layout]*)
let refresh layout =
  let i = W.image "bin/risk.png" ~noscale:true in
  let image = L.resident i in
  L.set_rooms layout ((image :: make_labels countries) @ color_labels ())

let start_fps, fps = Time.adaptive_fps 60

(*[update layout renderer board] refreshes [layout] and updates the gui*)
let update layout renderer board =
  refresh layout;
  Draw.set_color renderer bg;
  go (Sdl.render_clear renderer);
  let () =
    Bogue.refresh_custom_windows board;
    try
      if
        not (Bogue.one_step true (start_fps, fps) board)
        (* one_step returns true if fps was executed *)
      then fps ()
    with
    | Bogue.Exit -> ()
    | e -> raise e
  in
  Sdl.render_present renderer

(*[start layout renderer board] allows players to place troops and then changes
  the game state to [Playing]*)
let start layout renderer board =
  let () =
    for i = 0 to Player.num_players - 1 do
      let () =
        print_endline ("Player " ^ string_of_int (i + 1) ^ " place troops!")
      in
      Turn.place_troops i (starting_troops - Turn.num_terrs_owned i);
      update layout renderer board
    done
  in
  let () = game_state := Playing in
  ()

exception EndGameLoop

(*[play layout renderer board] plays through one players turn, updating the gui
  at each stage via [renderer]*)
let play layout renderer board =
  try
    for player = 0 to Player.num_players - 1 do
      print_endline ("Player " ^ string_of_int (player + 1) ^ " turn: ");
      check_end_condition ();
      if !game_state = Ending then begin
        summarize_game ();
        end_game ();
        raise EndGameLoop
      end;
      let troops = Turn.troops_on_start player in
      Turn.place_troops player troops;
      update layout renderer board;
      Turn.attack player;
      update layout renderer board;
      Turn.fortify player;
      update layout renderer board
    done
  with EndGameLoop -> ()

(*[run layout renderer board] matches against the stage of the game, runs that
  step*)
let run layout renderer board =
  try
    let () =
      match !game_state with
      | Starting -> start layout renderer board
      | Playing -> play layout renderer board
      | Ending -> end_game ()
    in
    refresh layout
  with _ -> raise Bogue.Exit

(*[mainloop layout renderer board] updates the gui with [renderer], runs one
  step of the game via [run],and calls its to continue to game loops *)
let rec mainloop layout renderer board =
  update layout renderer board;
  run layout renderer board;
  mainloop layout renderer board

(*[main () creates a game window, initializes a game board, and runs the game
  loop]*)
let main () =
  (*create gui*)
  Sys.catch_break false;
  go (Sdl.init Sdl.Init.video);
  Draw.video_init ();
  let w, h = (1000, 687) in
  let win =
    go
      (Sdl.create_window ~w ~h "Risk"
         Sdl.Window.(windowed + allow_highdpi + opengl + resizable))
  in
  let renderer = go (Sdl.create_renderer win) in
  go (Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_blend);
  Draw.set_color renderer bg;
  go (Sdl.render_clear renderer);

  (*Create widgets,layout and board*)
  let i = W.image "bin/risk.png" ~noscale:true in
  let image = L.resident i in
  let clabels = make_labels countries in
  let labels = image :: clabels in
  let layout = L.superpose ~background:(L.color_bg bg) labels in
  let board = Bogue.of_layout layout in
  Bogue.make_sdl_windows ~windows:[ win ] board;
  start_fps ();
  refresh layout;

  (*call main loop*)
  let () =
    try mainloop layout renderer board with
    | Sys.Break -> print_endline "Stop"
    | Bogue.Exit -> ()
    | e -> raise e
  in
  (*Closes games window*)
  Sdl.destroy_window win;
  Draw.quit ()

(*Executes main and closes gui*)
let () =
  main ();
  Bogue.quit ()
