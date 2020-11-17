%%%-------------------------------------------------------------------
%%% Created : 29 Oct 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(applev_receipt).

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
         start_link/3,
         terminate/2,
         handle_cast/2,
         handle_info/2,
         handle_call/3,
         code_change/3]).

% These functions are not part of gen_server
-export([maybe_json_decode/1]).

%%%===================================================================
%%% Local Defines
%%%===================================================================

%% Timeout to retry
-define(APPLE_WAIT_TIME, 100). % 100ms

%% Maximum number of retries
-define(MAX_RETRIES, 3).

% State Map description
-define(MAP_RECEIPT,      receipt).
-define(MAP_PID_DEST,     pid_dest).
-define(MAP_RETRIES,      retries).
-define(MAP_RCPT_ARGS,    args).
-define(MAP_APPLE_RCPT,   apple_receipt).

% Apple answer map
-define(MAP_APPLE_STATUS,  <<"status">>).    % From Apple
-define(MAP_APPLE_RECEIPT, <<"receipt">>).   % From Apple

% Http json Encode data
-define(APPLE_JSON_DATA, <<"receipt-data">>). % From apple

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link(pid() | atom(), binary(), any()) -> {ok, pid()} | ignore | {error, term()}.
start_link(PidDest, ReceiptToValidate, Args) ->
  gen_server:start_link(?MODULE, [PidDest, ReceiptToValidate, Args], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init(list()) -> {ok, map()} | ignore | {stop, term()}.
init([PidDest, ReceiptToValidate, Args]) ->
  %% Enable trapping exits
  process_flag(trap_exit, true),
  %% Start Validation
  erlang:send(self(), validation_start),
  %% Initiate the map with default values.
  {ok, #{?MAP_RETRIES    => ?MAX_RETRIES,
         ?MAP_PID_DEST   => PidDest,
         ?MAP_RECEIPT    => ReceiptToValidate,
         ?MAP_APPLE_RCPT => undefined,
         ?MAP_RCPT_ARGS  => Args} }.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

% Handle maximum number of retries.
handle_info(_, #{?MAP_RETRIES := 0, ?MAP_PID_DEST := PidDest,
                 ?MAP_RCPT_ARGS := Args } = State) ->
  erlang:send(PidDest, ?APPLEV_MSG(error, max_number_of_tries, Args)),
  {stop, normal, State};

% Handle validation
handle_info(validation_start, #{?MAP_RECEIPT := ReceiptToValidate,
                                ?MAP_RETRIES := Retries} = State) ->
  %% Validate receipt
  case request_apple_to_validate(?APPLE_PRODUCTION, ReceiptToValidate) of
    {ok, Receipt} ->
      erlang:send(self(), validation_done),
      {noreply, State#{?MAP_APPLE_RCPT => Receipt}};
    { error, _ } -> % Try again ...
      erlang:send_after(?APPLE_WAIT_TIME, self(), validation_start),
      {noreply, State#{?MAP_RETRIES => Retries - 1}}
  end;

% Handle results
handle_info(validation_done, #{?MAP_APPLE_RCPT :=
                               #{ ?MAP_APPLE_STATUS  :=
                                                  ?APPLE_STATUS_ERR_CORRUPTED },
                               ?MAP_PID_DEST  := PidDest,
                               ?MAP_RCPT_ARGS := Args }
                               = State) ->
  erlang:send(PidDest, ?APPLEV_MSG(error, receipt_corrupted, Args)),
  {stop, normal, State};

handle_info(validation_done, #{?MAP_APPLE_RCPT :=
                               #{ ?MAP_APPLE_STATUS  :=
                                                    ?APPLE_STATUS_ERR_EXPIRED },
                               ?MAP_PID_DEST  := PidDest,
                               ?MAP_RCPT_ARGS := Args }
                               = State) ->
  erlang:send(PidDest, ?APPLEV_MSG(error, receipt_expired, Args)),
  {stop, normal, State};

handle_info(validation_done, #{?MAP_APPLE_RCPT :=
                                  #{ ?MAP_APPLE_RECEIPT := Receipt},
                               ?MAP_PID_DEST  := PidDest,
                               ?MAP_RCPT_ARGS := Args } = State) ->
  erlang:send(PidDest, ?APPLEV_MSG(ok, Receipt, Args)),
  {stop, normal, State};

handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(normal, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Validate the receipt
%%
%%      From Apple website:
%%      Verify your receipt first with the production URL;
%%      proceed to verify with the sandbox URL if you receive a
%%      21007 status code. Following this approach ensures that
%%      you do not have to switch between URLs while your
%%      application is tested, reviewed by App Review, or live
%%      in the App Store.
%%
%% @param Url Website to be checked against
%% @param Receipt Receipt to be validated
%% @end
%%--------------------------------------------------------------------
-spec request_apple_to_validate(string(), binary()) ->
          {ok | error, map() | timeout | json}.
request_apple_to_validate(Url, Receipt) ->
  %% Prepare message
  Body    = jsone:encode( #{?APPLE_JSON_DATA => Receipt} ),
  Request = {Url, [], ?APPLE_CONTENT_TYPE, Body},

  InAppResponse = case httpc:request(post, Request, [], []) of
    {ok, {?HTTP_OK,_,Res} } -> Res;
    %% In case of httpc error, the behaviour will be the same as apple
    %% response for try again
     _ ->
         "{\"status\":21005}"
  end,

  %% Check specific cases for sandbox and retries.
  case maybe_json_decode(InAppResponse) of
    %% Redirect to send box
    { ok, #{?MAP_APPLE_STATUS := ?APPLE_STATUS_SANDBOX} } ->
      request_apple_to_validate(?APPLE_SAND_BOX, Receipt);
    %% Server error, try again
    { ok, #{?MAP_APPLE_STATUS := ErrorCode} }
      when ErrorCode =:= ?APPLE_STATUS_ERR_SERVER_1 ;
           ErrorCode =:= ?APPLE_STATUS_ERR_SERVER_2 ->
      { error, timeout };
    %% Return result
    Result ->
      Result
  end.

%%--------------------------------------------------------------------
%% @doc Try to decode json string
%%
%% @param String String to be decoded
%% @end
%%--------------------------------------------------------------------
-spec maybe_json_decode(string()) -> {ok | error, map() | json}.
maybe_json_decode(Str) ->
  try jsone:decode(erlang:list_to_binary(Str)) of
    DecodedStr -> {ok, DecodedStr}
  catch
    _:_ ->
        {error, json}
  end.