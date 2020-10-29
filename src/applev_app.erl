%%%-------------------------------------------------------------------
%% @doc applev public API
%% @end
%%%-------------------------------------------------------------------

-module(applev_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    applev_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
