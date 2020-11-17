%%%-------------------------------------------------------------------
%%% Created : 29 Oct 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(applev_receipt_consumer).

-behaviour(gen_server).

-author('Thiago Esteves').

%%%===================================================================
%%% Includes
%%%===================================================================

-include("applev.hrl").

%%%===================================================================
%%% Function exports
%%%===================================================================

%% gen_server exports
-export([init/1,
         start_link/0,
         start_link/1,
         terminate/2,
         handle_cast/2,
         handle_info/2,
         handle_call/3,
         code_change/3]).

%% API
-export([get_receipt/1]).

%%%===================================================================
%%% Global Defines
%%%===================================================================

%% Default timeout to wait for a message (from call and info)
-define(TIMEOUT_TO_SHUTDOWN, ?DEFAULT_TIMEOUT_FOR_VALIDATION).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link(?MODULE, [?TIMEOUT_TO_SHUTDOWN], []).

-spec start_link(integer()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Timeout) ->
  gen_server:start_link(?MODULE, [Timeout], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init(list()) -> {ok, map(), non_neg_integer()}.
init([Timeout]) ->
  {ok, #{timeout => Timeout, status => error, receipt => undefined}, Timeout}.

handle_cast(_Msg, State) ->
  {stop, normal, State}.

handle_call(_Msg, _From, #{ timeout := Timeout,
                            receipt := undefined} = State) ->
  {reply, { ok, undefined }, State, Timeout};

handle_call(get_receipt, _From, #{ receipt := R,
                                   status  := S} = State) ->
  {stop, normal, { S, R }, State}.

handle_info( {apple_receipt_return, {Status, Rcpt, _Args} }, #{ timeout := T } = State) ->
  {noreply, State#{ status => Status, receipt => Rcpt }, T};

handle_info(timeout, State) ->
  {stop, normal, State}.

%% @private
terminate(normal, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

-spec get_receipt(pid()) -> { ok | error , apple_return() | map() }.
get_receipt(Pid) ->
  gen_server:call(Pid, get_receipt).

%%====================================================================
%% Internal functions
%%====================================================================
