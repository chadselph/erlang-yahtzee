%% Copyright
-author("chadrs").

-type combination() :: ones | twos | threes | fours | fives | sixes |
three_of_a_kind | four_of_a_kind | full_house |
small_straight | large_straight | yahtzee | chance.

-type scoreboard() :: [{combination(), non_neg_integer() | not_played_yet}].

-define(InitialScorecard, [
  {ones, not_played_yet},
  {twos, not_played_yet},
  {threes, not_played_yet},
  {fours, not_played_yet},
  {fives, not_played_yet},
  {sixes, not_played_yet},
  {three_of_a_kind, not_played_yet},
  {four_of_a_kind, not_played_yet},
  {full_house, not_played_yet},
  {small_straight, not_played_yet},
  {large_straight, not_played_yet},
  {yahtzee, not_played_yet},
  {chance, not_played_yet}
]).

-define(UpperBonus, 35).
-define(UpperBonusThreshold, 63).
-define(UpperCombinations, [ones, twos, threes, fours, fives, sixes]).
-define(LowerCombinations, [three_of_a_kind, four_of_a_kind, full_house,
  small_straight, large_straight, yahtzee, chance]).