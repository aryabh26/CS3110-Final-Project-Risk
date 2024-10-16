open Player

(* second unit function that runs in the beginning of the game. picks bonus
   mode*)
let rec cardModeSelector () =
  let () = print_endline "Choose between progressive/constant card bonuses" in
  let input = read_line () in
  if input <> "progressive" && input <> "constant" then cardModeSelector ()
  else input

(* [card_mode] is the choice of game mode that determines bonuses of the game.
   set in the beginning, and accessed at awards of bonuses*)
let card_mode = cardModeSelector ()
let contant_bonus = 10

(* [progressive_bonus] is int array of how bonuses increase*)
let progressive_bonus = [| 4; 6; 8; 10; 12; 15; 20; 25; 30 |]

(* [bonus_counter] is a counter for the bonuses awarded*)
let bonus_counter = ref 0
let bank = [ ("art", ref 14); ("cav", ref 14); ("inf", ref 14) ]

(* indexes player cards by poition and iw*)
let player_cards =
  Array.make num_players [ ("art", ref 0); ("cav", ref 0); ("inf", ref 0) ]

(* [pick_card] is unit function that when called picks a card randomly, outputs
   it as string, so that it could be used to update player's cards. It updates
   states appropriately*)
let pick_card () =
  let total_cards =
    !(List.assoc "art" bank)
    + !(List.assoc "cav" bank)
    + !(List.assoc "inf" bank)
  in
  if total_cards = 0 then "empty" (* No cards left to pick *)
  else
    let determine_card pick =
      if pick <= !(List.assoc "art" bank) then begin
        List.assoc "art" bank := !(List.assoc "art" bank) - 1;
        (* Decrease artillery count *)
        "art"
      end
      else if pick <= !(List.assoc "art" bank) + !(List.assoc "cav" bank) then begin
        List.assoc "cav" bank := !(List.assoc "cav" bank) - 1;
        (* Decrease cavalry count *)
        "cav"
      end
      else begin
        List.assoc "inf" bank := !(List.assoc "inf" bank) - 1;
        (* Decrease infantry count *)
        "inf"
      end
    in
    let pick = Random.int total_cards + 1 in
    (* Random number from 1 to total_cards *)
    determine_card pick

(* Function to calculate the bonus troops *)
let calculate_bonus () =
  match card_mode with
  | "constant" -> 10
  | "progressive" ->
      let max_index = Array.length progressive_bonus - 1 in
      let bonus =
        if !bonus_counter > max_index then progressive_bonus.(max_index)
        else progressive_bonus.(!bonus_counter)
      in
      if !bonus_counter <= max_index then bonus_counter := !bonus_counter + 1;
      bonus
  | _ -> failwith "Unexpected card mode"

(* [award_bonus] is a function that awards bonuses to the player based on the
   chosen mode *)
let award_bonus_card player_index =
  (* Pick a card *)
  let card = pick_card () in

  (* Update the player's cards in the player_cards array based on the card
     picked *)
  match card with
  | "art" ->
      List.assoc "art" player_cards.(player_index)
      := !(List.assoc "art" player_cards.(player_index)) + 1
  | "cav" ->
      List.assoc "cav" player_cards.(player_index)
      := !(List.assoc "cav" player_cards.(player_index)) + 1
  | "inf" ->
      List.assoc "inf" player_cards.(player_index)
      := !(List.assoc "inf" player_cards.(player_index)) + 1
  | "empty" -> print_endline "No cards left to pick"
  | _ -> failwith "Unexpected card type"

let view_cards player_index =
  let art_cards = !(List.assoc "art" player_cards.(player_index)) in
  let cav_cards = !(List.assoc "cav" player_cards.(player_index)) in
  let inf_cards = !(List.assoc "inf" player_cards.(player_index)) in
  Printf.printf "Player %d has:\n" player_index;
  Printf.printf "Artillery: %d\n" art_cards;
  Printf.printf "Cavalry: %d\n" cav_cards;
  Printf.printf "Infantry: %d\n" inf_cards

let deploy_cards turn_state =
  let player_index = turn_state in
  let art_cards = !(List.assoc "art" player_cards.(player_index)) in
  let cav_cards = !(List.assoc "cav" player_cards.(player_index)) in
  let inf_cards = !(List.assoc "inf" player_cards.(player_index)) in
  let has_one_of_each = art_cards >= 1 && cav_cards >= 1 && inf_cards >= 1 in
  let has_three_of_a_kind =
    art_cards >= 3 || cav_cards >= 3 || inf_cards >= 3
  in
  if has_one_of_each || has_three_of_a_kind then
    let () = print_endline "Do you want to deploy troops? (yes/no)" in
    let input = read_line () in
    if input = "yes" then
      let () = view_cards player_index in
      let () =
        print_endline
          "Enter the combination to deploy (e.g., 'art art art' or 'art cav \
           inf')"
      in
      let combination = read_line () in
      let cards = String.split_on_char ' ' combination in
      if
        List.length cards = 3
        && ((List.for_all (( = ) "art") cards && art_cards >= 3)
           || (List.for_all (( = ) "cav") cards && cav_cards >= 3)
           || (List.for_all (( = ) "inf") cards && inf_cards >= 3)
           || List.mem "art" cards && List.mem "cav" cards
              && List.mem "inf" cards && has_one_of_each)
      then begin
        List.iter
          (fun card ->
            match card with
            | "art" ->
                List.assoc "art" player_cards.(player_index)
                := !(List.assoc "art" player_cards.(player_index)) - 1;
                List.assoc "art" bank := !(List.assoc "art" bank) + 1
            | "cav" ->
                List.assoc "cav" player_cards.(player_index)
                := !(List.assoc "cav" player_cards.(player_index)) - 1;
                List.assoc "cav" bank := !(List.assoc "cav" bank) + 1
            | "inf" ->
                List.assoc "inf" player_cards.(player_index)
                := !(List.assoc "inf" player_cards.(player_index)) - 1;
                List.assoc "inf" bank := !(List.assoc "inf" bank) + 1
            | _ -> failwith "Invalid card type")
          cards;
        let bonus = calculate_bonus () in
        Printf.printf
          "Player %d deploys cards and receives a bonus of %d troops\n"
          player_index bonus
      end
      else print_endline "Invalid combination or insufficient cards"
    else print_endline "Deployment canceled"
  else print_endline "Insufficient cards to deploy"

let check_and_view_cards index =
  let art_cards = !(List.assoc "art" player_cards.(index)) in
  let cav_cards = !(List.assoc "cav" player_cards.(index)) in
  let inf_cards = !(List.assoc "inf" player_cards.(index)) in
  let total_cards = art_cards + cav_cards + inf_cards in
  if total_cards >= 1 then
    let () = print_endline "Do you want to view your cards? (yes/no)" in
    let input = read_line () in
    if input = "yes" then view_cards index
