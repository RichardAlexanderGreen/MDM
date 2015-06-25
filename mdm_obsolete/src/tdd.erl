%% Author: admin
%% Created: Nov 26, 2010
%% Description: TODO: Add description to tdd
-module(tdd).

%%
%% Include files
%%
-define( TEST, true ).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%



%%
%% Local Functions
%%
reverse_nil_test() -> [] = lists:reverse([]).
reverse_one_test() -> [1] = lists:reverse([1]).
reverse_two_test() -> [2,1] = lists:reverse([1,2]).
