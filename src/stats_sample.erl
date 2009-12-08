%% -------------------------------------------------------------------
%%
%% stats: Statistics Suite for Erlang
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(stats_sample).

-export([new/0,
         update/2,
         count/1,
         min/1, mean/1, max/1,
         variance/1, sdev/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, { n = 0,
                 min,
                 max,
                 sum  = 0,
                 sum2 = 0 }).


%% ===================================================================
%% Public API
%% ===================================================================

new() ->
    #state{}.

update(Value, State) ->
    State#state {
      n   = State#state.n + 1,
      min = erlang:min(Value, State#state.min),
      max = erlang:max(Value, State#state.max),
      sum = State#state.sum + Value,
      sum2= State#state.sum + (Value * Value)}.


count(State) ->
    State#state.n.

min(State) ->
    State#state.min.

mean(#state{n = 0}) ->
    'NaN';
mean(State) ->
    State#state.sum / State#state.n.

max(State) ->
    State#state.max.

variance(#state { n = N}) when N < 2 ->
    'NaN';
variance(State) ->
    SumSq = State#state.sum * State#state.sum,
    (State#state.sum2 - (SumSq / State#state.n)) / (State#state.n - 1).


sdev(State) ->
    case variance(State) of
        'NaN' ->
            'NaN';
        Value ->
            math:sqrt(Value)
    end.
            


%% ===================================================================
%% Unit Tests
%% ===================================================================

-ifdef(EUNIT).

simple_test() ->
    {ok, [1,2,3,3,4,5]} = stats_utils:r_run([1,2,3,4,5], "summary(x)"),
    {ok, [0,25,50,50,75,100]} = stats_utils:r_run(lists:seq(0,100), "summary(x)").

-endif.
