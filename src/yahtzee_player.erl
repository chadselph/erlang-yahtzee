%% Copyright
-module(yahtzee_player).
-author("chadrs").

-behaviour(gen_fsm).

-include("yahtzee_rules.hrl").

%% API
-export([start_link/0, roll_dice/2, choose_combination/2]).

%% gen_fsm
-export([init/1, handle_event/3,
  handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% API
start_link() ->
  gen_fsm:start_link(?MODULE, [], []).

-spec roll_dice(pid(), [boolean()]) -> term().
roll_dice(Pid, {roll, HoldOrRoll) ->
  gem_fsm:sync_send_event(Pid, HoldOrRoll).

-spec choose_combination(pid(), combination()) -> term().
choose_combination(Pid, Combination) ->
  gen_fsm:sync_send_event(Pid, Combination).


%% gen_fsm callbacks
-record(state, {scorecard=?InitialScorecard, dice=none}).

init(_Args) ->
  {ok, first_roll, #state{}}.

first_roll({roll, Dice}, State) ->
  {next_state, second_roll, State}.

second_roll(_Event,_From, State) ->
  {reply, ok, state_name, State}.

final_roll(

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.
