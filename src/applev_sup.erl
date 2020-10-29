%%%-------------------------------------------------------------------
%%% Created : 29 Oct 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc
%%% This is the Apple validation top level supervisor
%%% @end
%%%-------------------------------------------------------------------

-module(applev_sup).

-author('Thiago Esteves').

%%====================================================================
%% API functions
%%====================================================================

-export([start_link/0]).

-export([init/1]).

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(MSG_PROCESS_SUP_NAME, applev_receipt_sup).

%%====================================================================
%% API functions implementation
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  SupFlags = #{strategy => one_for_one,
               intensity => 4,
               period => 30},

  ChildSpecs = [#{id => ?MSG_PROCESS_SUP_NAME,
                  start => {?MSG_PROCESS_SUP_NAME, start_link, []},
                  restart => permanent,
                  type => supervisor,
                  shutdown => brutal_kill} ],
  {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
