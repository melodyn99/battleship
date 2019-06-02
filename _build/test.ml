open OUnit2
open Move
open Ai
open State

let move_tests =
  [
    "test parse 1" >:: (fun _ ->
        assert_equal (Quit) (parse " quit    "));
    "test parse 2" >:: (fun _ -> assert_raises (Empty) (fun() -> parse ""));
    "test parse 3" >:: (fun _ ->
        assert_equal (Attack ("B1")) (parse "attack B1"));
    "test parse 4" >:: (fun _ ->
        assert_raises (Illegal) (fun() -> parse "afsdjkl afjsd"));
    "test parse 5" >:: (fun _ ->
        assert_equal (Place ("carrier", "H", "B4")) (parse "place carrier H B4    "));
    "test parse 6" >:: (fun _ ->
        assert_equal (Place ("carrier", "H", "B4")) (parse "   place     carrier H   B4"));
    "test parse 7" >:: (fun _ ->
        assert_raises (Empty) (fun() -> parse " "));
    "test parse 8" >:: (fun _ ->
        assert_raises (Illegal) (fun() -> parse "place pizza"));
    "test parse 9" >:: (fun _ ->
        assert_raises (Illegal) (fun() -> parse "attack carrier H "));
    "test parse 10" >:: (fun _ ->
        assert_raises (Illegal) (fun() -> parse "place something here"));
    "test parse 11" >:: (fun _ ->
        assert_raises (Illegal) (fun() -> parse "Place carrier H B4    "));

  ]

let initial_state = init_state "Joe"
let next_state = player_place initial_state "carrier" "H" "B4"
let next_state2 = player_attack next_state "B4"
let next_state3 = player_place next_state2 "destroyer" "H" "A1"
let next_state4 = player_place next_state3 "submarine" "H" "C1"
let next_state5 = player_place next_state4 "cruiser" "H" "D1"
let next_state6 = player_place next_state5 "battleship" "H" "E1"
let pos1 = helper "A1"
let pos2 = helper "B13"
let state_tests =
  [
    "test player name" >:: (fun _ ->
        assert_equal "Joe" (get_name initial_state));
    "test ai win 1" >:: (fun _ -> assert_equal false (ai_win initial_state));
    "test player win 1" >:: (fun _ ->
        assert_equal false (player_win initial_state));
    "test remaining 1" >:: (fun _ -> assert_equal 2 (remaining initial_state));
    "test get_enemy_hits 1" >:: (fun _ ->
        assert_equal 0 (get_enemy_hits initial_state));
    "test ai win 2" >:: (fun _ -> assert_equal false (ai_win next_state));
    "test player win 2" >:: (fun _ ->
        assert_equal false (player_win next_state));
    "test remaining 2" >:: (fun _ ->
        assert_equal 2 (remaining next_state3));
    "test remaining 3" >:: (fun _ ->
        assert_equal 2 (remaining next_state4));
    "test remaining 4" >:: (fun _ ->
        assert_equal 1 (remaining next_state5));
    "test remaining 5" >:: (fun _ ->
        assert_equal 0 (remaining next_state6));
    "test get_enemy_hits 2" >:: (fun _ ->
        assert_equal 0 (get_enemy_hits next_state));
    "test ai win 3" >:: (fun _ -> assert_equal false (ai_win next_state2));
    "test player win 3" >:: (fun _ ->
        assert_equal false (player_win next_state2));
    "test get_enemy_hits 3" >:: (fun _ ->
        assert_equal 0 (get_enemy_hits next_state2));
    "test in_bounds 1" >:: (fun _ ->
        assert_equal true (in_bounds pos1));
    "test in_bounds 2" >:: (fun _ ->
        assert_equal false (in_bounds pos2));
    "test length of ship 1" >:: (fun _ ->
        assert_equal 5 (length_of_ship "carrier"));
    "test length of ship 2" >:: (fun _ ->
        assert_equal 2 (length_of_ship "destroyer"));
    "test length of ship 3" >:: (fun _ ->
        assert_raises (InvalidShipName) (fun() -> length_of_ship "joe"));
    "test length of ship 4" >:: (fun _ ->
        assert_equal 4 (length_of_ship "battleship"));
    "test length of ship 5" >:: (fun _ ->
        assert_equal 3 (length_of_ship "cruiser"));
    "test length of ship 6" >:: (fun _ ->
        assert_equal 3 (length_of_ship "submarine"));
  ]
let first_ai = init_ai "master"
let second_ai = init_ai "professional"
let third_ai =
  match (attack second_ai initial_state) with
  |(_, y) -> y

let ai_tests =
  [
    "test get level 1" >:: (fun _ ->
        assert_equal "master" (get_level first_ai));
    "test get level 2" >:: (fun _ ->
        assert_equal "professional" (get_level second_ai));
    "test get level 3" >:: (fun _ ->
        assert_equal "professional" (get_level third_ai));
  ]

let suite =
  "test suite for A7"  >::: List.flatten [
    move_tests;
    state_tests;
    ai_tests;
  ]

let _ = run_test_tt_main suite
