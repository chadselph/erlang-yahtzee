%% Copyright
-module(yahtzee_rules).
-author("chadrs").

-include("yahtzee_rules.hrl").

%% API
-export([score_for_combination/2, is_game_over/1, total_score/1,
         lower_score/1, upper_score/1, choose_combination/3]).

-spec is_game_over(scoreboard()) -> boolean().
is_game_over(Scorecard) ->
  not(lists:any(fun
    ({_, not_played_yet}) -> true;
    (_) -> false
  end, Scorecard)).

upper_score(Scorecard) ->
  UpperTotal = scorecard_sum_fields(Scorecard, ?UpperCombinations),
  case UpperTotal of
    Score when Score >= ?UpperBonusThreshold -> Score + ?UpperBonus;
    Score -> Score
  end.

lower_score(Scorecard) -> scorecard_sum_fields(Scorecard, ?LowerCombinations).

total_score(Scorecard) -> lower_score(Scorecard) + upper_score(Scorecard).


choose_combination(Scorecard, Choice, Hand) ->
  case proplists:get_value(Choice, Scorecard) of
    Score when is_integer(Score) -> {error, already_played};
    not_played_yet ->
      lists:keyreplace(Choice, 1, Scorecard, {Choice, score_for_combination(Choice, Hand)})
  end.

%% Upper Section
-spec score_for_combination(combination(), [1..6]) -> non_neg_integer().
score_for_combination(ones, Hand) -> sum_all_the(1, Hand);
score_for_combination(twos, Hand) -> sum_all_the(2, Hand);
score_for_combination(threes, Hand) -> sum_all_the(3, Hand);
score_for_combination(fours, Hand) -> sum_all_the(4, Hand);
score_for_combination(fives, Hand) -> sum_all_the(5, Hand);
score_for_combination(sixes, Hand) -> sum_all_the(6, Hand);

%% Lower Section
score_for_combination(three_of_a_kind, Hand) -> score_n_of_a_kind(3, Hand);
score_for_combination(four_of_a_kind, Hand) -> score_n_of_a_kind(4, Hand);
score_for_combination(full_house, Hand) ->
  case lists:sort(Hand) of
    [X, X, X, Y, Y] -> 25;
    [X, X, Y, Y, Y] -> 25;
    _ -> 0
  end;
score_for_combination(small_straight, Hand) ->
  case longest_sequence(Hand) of
    N when N >= 4 -> 30;
    _ -> 0
  end;
score_for_combination(large_straight, Hand) ->
  case lists:usort(Hand) of
    [1, 2, 3, 4, 5] -> 40;
    [2, 3, 4, 5, 6] -> 40;
    _ -> 0
  end;
score_for_combination(yahtzee, [X, X, X, X, X]) -> 50;
score_for_combination(yahtzee, _) -> 0;
score_for_combination(chance, Hand) -> lists:sum(Hand).

%%%%% scorecard private functions %%%%%%%
scorecard_sum_fields(Scorecard, Fields) ->
  lists:foldl(fun (Combo, Acc) ->
    case proplists:get_value(Combo, Scorecard) of
      not_played_yet -> Acc;
      Points when is_integer(Points) -> Acc + Points
    end
  end, 0, Fields).

%%%%% misc private functions %%%%%%%
score_n_of_a_kind(N, Hand) ->
  case n_of_any(N, Hand) of
    false -> 0;
    _ -> lists:sum(Hand)
  end.

sum_all_the(_Number, []) -> 0;
sum_all_the(Number, [Number|Rest]) -> Number + sum_all_the(Number, Rest);
sum_all_the(Number, [_|Rest]) -> sum_all_the(Number, Rest).

n_of_any(N, List) -> n_of_any(N, List, dict:new()).
n_of_any(_, [], _) -> false;
n_of_any(N, [Head|Tail], Counters) ->
  NCounters = dict:update_counter(Head, 1, Counters),
  case dict:fetch(Head, NCounters) of
    Count when Count >= N -> Head;
    _ -> n_of_any(N, Tail, NCounters)
  end.

longest_sequence(List) ->
  [X|Xs] = lists:usort(List),
  longest_sequence(X, Xs, 1, 1).

longest_sequence(_, [], _, Longest) -> Longest;
longest_sequence(X, [Y|Rest], Current, Longest) when X + 1 =:= Y ->
  longest_sequence(Y, Rest, Current + 1, max(Current + 1, Longest));
longest_sequence(_, [Y| Rest], _, Longest) -> longest_sequence(Y, Rest, 1, Longest).
