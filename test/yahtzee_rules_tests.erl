%% Copyright
-module(yahtzee_rules_tests).
-author("chadrs").

-include("yahtzee_rules.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(_assertScores(Combo, Spec),
  lists:map(fun ({Expected, Dice}) ->
    {
      lists:flatten(io_lib:format("~p with ~p", [Combo, Dice])),
      ?_assertEqual(Expected, yahtzee_rules:score_for_combination(Combo, Dice))
    }
  end, Spec)
).

game_over_test_() ->
  [
    {"Initial game is not over",
    ?_assertEqual(false, yahtzee_rules:is_game_over(?InitialScorecard))},
    {"Mostly over game is not over",
    ?_assertEqual(false, yahtzee_rules:is_game_over([
      {ones, 1},
      {twos, 8},
      {threes, not_played_yet},
      {fours, 20},
      {fives, 10},
      {sixes, 24},
      {three_of_a_kind, 15},
      {four_of_a_kind, 10},
      {full_house, 25},
      {small_straight, 30},
      {large_straight, 40},
      {yahtzee, 50},
      {chance, not_played_yet}
    ]))},
    {"Completed game is over",
    ?_assertEqual(true, yahtzee_rules:is_game_over([
      {ones, 1},
      {twos, 8},
      {threes, 9},
      {fours, 20},
      {fives, 10},
      {sixes, 24},
      {three_of_a_kind, 15},
      {four_of_a_kind, 10},
      {full_house, 25},
      {small_straight, 30},
      {large_straight, 40},
      {yahtzee, 50},
      {chance, 10}
    ]))}
  ].

combinations_test_() ->
  [
    ?_assertScores(ones, [
      {5, [1,1,1,1,1]},
      {0, [5,4,3,2,2]},
      {2, [5,4,3,1,1]}
    ]),
    ?_assertScores(twos, [
      {0, [1,1,1,1,1]},
      {4, [5,4,3,2,2]},
      {10, [2,2,2,2,2]}
    ]),
    ?_assertScores(threes, [
      {0, [1,1,1,1,1]},
      {3, [5,4,3,2,2]},
      {12, [5,3,3,3,3]}
    ]),
    ?_assertScores(fours, [
      {4, [1,1,4,1,1]},
      {8, [5,4,4,2,2]},
      {0, [5,3,3,1,1]}
    ]),
    ?_assertScores(fives, [
      {5, [1,5,1,1,1]},
      {20, [5,4,5,5,5]},
      {0, [2,4,3,1,1]}
    ]),
    ?_assertScores(sixes, [
      {30, [6,6,6,6,6]},
      {0, [5,4,3,2,2]},
      {18, [6,4,6,1,6]}
    ]),
    ?_assertScores(three_of_a_kind, [
      {30, [6,6,6,6,6]},
      {10, [1, 1, 1, 4, 3]},
      {0, [6,5,5,4,4]}
    ]),
      ?_assertScores(four_of_a_kind, [
      {30, [6,6,6,6,6]},
      {8, [1, 1, 1, 4, 1]},
      {0, [6,5,5,5,4]}
    ]),
    ?_assertScores(full_house, [
      {25, [6,6,6,6,6]},
      {0, [1, 1, 1, 4, 1]},
      {0, [6,5,5,5,4]},
      {25, [1,2,1,2,1]},
      {25, [4,5,5,5,4]}
    ]),
    ?_assertScores(small_straight, [
      {30, [1,2,3,4,5]},
      {30, [6,5,3,4,1]},
      {30, [2,3,2,4,5]},
      {0, [1,2,3,5,6]},
      {0, [1,4,2,2,1]}
    ]),
    ?_assertScores(large_straight, [
      {40, [1,2,3,4,5]},
      {40, [6,5,3,4,2]},
      {0, [2,3,2,4,5]},
      {0, [1,2,3,5,6]},
      {0, [1,4,2,2,1]}
    ]),
    ?_assertScores(yahtzee, [
      {50, [2,2,2,2,2]},
      {50, [6,6,6,6,6]},
      {0, [2,3,2,4,5]},
      {0, [1,1,1,1,2]}
    ]),
    ?_assertScores(chance, [
      {26, [5, 5, 5, 5, 6]},
      {15, [4,3,2,1, 5]}
    ])
  ].

upper_score_no_bonus_test() ->
  ?assertEqual(62, yahtzee_rules:upper_score([
    {ones, not_played_yet},
    {twos, 8},
    {threes, not_played_yet},
    {fours, 20},
    {fives, 10},
    {sixes, 24},
    {three_of_a_kind, not_played_yet},
    {four_of_a_kind, not_played_yet},
    {full_house, not_played_yet},
    {small_straight, 30},
    {large_straight, 40},
    {yahtzee, 50},
    {chance, not_played_yet}
  ])).

upper_score_bonus_test() ->
  ?assertEqual(98, yahtzee_rules:upper_score([
    {ones, 1},
    {twos, 8},
    {threes, not_played_yet},
    {fours, 20},
    {fives, 10},
    {sixes, 24},
    {three_of_a_kind, not_played_yet},
    {four_of_a_kind, not_played_yet},
    {full_house, not_played_yet},
    {small_straight, 30},
    {large_straight, 40},
    {yahtzee, 50},
    {chance, not_played_yet}
  ])).

lower_score_test() ->
  ?assertEqual(170, yahtzee_rules:lower_score([
    {ones, 1},
    {twos, 8},
    {threes, not_played_yet},
    {fours, 20},
    {fives, 10},
    {sixes, 24},
    {three_of_a_kind, 15},
    {four_of_a_kind, 10},
    {full_house, 25},
    {small_straight, 30},
    {large_straight, 40},
    {yahtzee, 50},
    {chance, not_played_yet}
  ])).

total_score_test_() ->
  [
    ?_assertEqual(268, yahtzee_rules:total_score([
      {ones, 1},
      {twos, 8},
      {threes, not_played_yet},
      {fours, 20},
      {fives, 10},
      {sixes, 24},
      {three_of_a_kind, 15},
      {four_of_a_kind, 10},
      {full_house, 25},
      {small_straight, 30},
      {large_straight, 40},
      {yahtzee, 50},
      {chance, not_played_yet}
    ])),
    ?_assertEqual(0, yahtzee_rules:total_score(?InitialScorecard))
  ].

choose_combination_test() ->
  Sc1 = [
    {ones, 1},
    {twos, 8},
    {threes, not_played_yet},
    {fours, 20},
    {fives, 10},
    {sixes, 24},
    {three_of_a_kind, 15},
    {four_of_a_kind, 10},
    {full_house, 25},
    {small_straight, 30},
    {large_straight, 40},
    {yahtzee, 50},
    {chance, not_played_yet}
  ],
  Dice = [5,5,5,5,5],
  Sc2 = yahtzee_rules:choose_combination(Sc1, chance, Dice),
  ?assertEqual(yahtzee_rules:total_score(Sc1) +
               yahtzee_rules:score_for_combination(chance, Dice),
               yahtzee_rules:total_score(Sc2)).

choose_combination_already_played_test() ->
  Sc1 = [
    {ones, 1},
    {twos, 8},
    {threes, not_played_yet},
    {fours, 20},
    {fives, 10},
    {sixes, 24},
    {three_of_a_kind, 15},
    {four_of_a_kind, 10},
    {full_house, 25},
    {small_straight, 30},
    {large_straight, 40},
    {yahtzee, 50},
    {chance, not_played_yet}
  ],
  ?assertEqual({error, already_played},
               yahtzee_rules:choose_combination(Sc1, ones, [1,1,1,1,1])).

