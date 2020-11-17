%%%-------------------------------------------------------------------
%%% Created : 29 Oct 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc This file contains the APIs for sync and async validation functions
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(applev).

-author('Thiago Esteves').

%%%===================================================================
%%% Includes
%%%===================================================================

-include("applev.hrl").

%%%===================================================================
%%% Function exports
%%%===================================================================

% Validate function (blocking)
-export([validate/0, validate/1]).

% Validate function (non-blocking)
-export([validate_async/0, validate_async/3]).

%%%===================================================================
%%% Local Defines
%%%===================================================================

-define(WAIT_TIME_TO_RETRY, 50). % 50ms

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc This function is going to use a sandbox receipt to test sync callings
%%
%% @end
%%--------------------------------------------------------------------
-spec validate() -> { ok | error , apple_return() | map() }.
validate()->
  validate(?APPLE_SANDBOX_RECEIPT).

%%--------------------------------------------------------------------
%% @doc This function validates the receipt via a blocking function
%%
%% @param BinReceipt Receipt to validate in binary format <<"example">>
%% @end
%%--------------------------------------------------------------------
-spec validate(binary()) -> { ok | error , apple_return() | map() }.
validate(BinReceipt) ->
  { ok, PidDest } = applev_receipt_consumer:start_link(
                                               ?DEFAULT_TIMEOUT_FOR_VALIDATION),
  { ok, _}        = applev_receipt_sup:process_msg(PidDest, BinReceipt, none),
  wait_web_request(PidDest).

%%--------------------------------------------------------------------
%% @doc This function is going to use a sandbox receipt and the sender
%%      pid to test async callings
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_async() -> { ok , undefined | pid() }.
validate_async()->
  %% Read file with a default sand-box receipt
  applev_receipt_sup:process_msg(self(), ?APPLE_SANDBOX_RECEIPT, none).

%%--------------------------------------------------------------------
%% @doc This function validates the receipt via a non-blocking function
%%
%% @param PidDest Process pid that will receive the result
%% @param BinReceipt Receipt to validate in binary format <<"example">>
%% @param Args Any Related data for the specific receipt
%% @end
%%--------------------------------------------------------------------
-spec validate_async(pid(), binary(), any()) -> { ok , undefined | pid() }.
validate_async(PidDest, BinReceipt, Args) ->
  applev_receipt_sup:process_msg(PidDest, BinReceipt, Args).

%%====================================================================
%% Internal functions
%%====================================================================
-spec wait_web_request(pid()) -> { ok | error , apple_return() | map() }.
wait_web_request(PidDest) ->
  case applev_receipt_consumer:get_receipt(PidDest) of
    {ok, undefined} -> timer:sleep(?WAIT_TIME_TO_RETRY),
                       wait_web_request(PidDest);
     R -> R
  end.