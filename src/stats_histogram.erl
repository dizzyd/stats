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
-module(stats_histogram).

-export([new/3,
         update/2,
         quantile/2]).

-record(hist, { n = 0,
                min,
                max,
                bin_scale,
                bins,
                capacity }).

%% ===================================================================
%% Public API
%% ===================================================================

new(MinVal, MaxVal, NumBins) ->
    #hist { min = MinVal,
            max = MaxVal,
            bin_scale = NumBins / (MaxVal - MinVal),
            bins = gb_trees:empty(),
            capacity = NumBins }.

%%
%% Update the histogram with a new observation.
%%
%% NOTE: update/2 caps values within #hist.min and #hist.max;
%% if you provide a value outside those boundaries the first or last
%% bin, respectively, get updated and the histogram is consequently
%% skewed.
%%
update(Value, Hist) ->
    Bin = which_bin(Value, Hist),
    case gb_trees:lookup(Bin, Hist#hist.bins) of
        {value, Counter} ->
            ok;
        none ->
            Counter = 1
    end,
    Hist#hist { n = Hist#hist.n + 1,
                bins = gb_trees:enter(Bin, Counter + 1, Hist#hist.bins) }.


%%
%% Estimate the quantile from the histogram. Quantile should be a value
%% between 0 and 1. Returns 'NaN' if the histogram is currently empty.
%%
quantile(_Quantile, #hist { n = 0 }) ->
    'NaN';
quantile(Quantile, Hist)
  when Quantile > 0; Quantile < 1 ->
    %% Sort out how many samples we need to satisfy the requested quantile
    MaxSamples = trunc(Quantile * Hist#hist.n),

    %% Now iterate over the bins, until we have gathered enough samples
    %% to satisfy the request. The resulting bin is an estimate.
    case quantile_itr(gb_trees:iterator(Hist#hist.bins), 0, MaxSamples) of
        max ->
            Hist#hist.max;
        EstBin ->
            %% We have an estimated bin -- determine the lower bound of said
            %% bin
            (EstBin / Hist#hist.bin_scale) - Hist#hist.min
    end.


%% ===================================================================
%% Internal functions
%% ===================================================================

which_bin(Value, Hist) ->
    Bin = trunc(Value * Hist#hist.bin_scale),
    if
        Bin < 0 ->
            0;
        Bin > Hist#hist.max ->
            Hist#hist.capacity - 1;
        true ->
            Bin
    end.

            
quantile_itr(none, _Samples, _MaxSamples) ->
    max;
quantile_itr({Bin, Counter, Itr2}, Samples, MaxSamples) ->
    Samples2 = Samples + Counter,
    if
        Samples2 < MaxSamples ->
            %% Not done yet, move to next bin
            quantile_itr(gb_trees:next(Itr2), Samples2, MaxSamples);
        Samples2 == MaxSamples ->
            %% This bin (in its entirety) satisfies our criteria
            Bin + 1;
        true ->
            %% We only need some of the samples in this bin; we make
            %% the assumption that values within the bin are uniformly
            %% distributed.
            Bin + ((MaxSamples - Samples) / Counter)
    end.
            
            
            
