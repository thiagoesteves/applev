%%%-------------------------------------------------------------------
%%% Created : 30 Oct 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc Test suite file
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(applev_test_SUITE).

%%%===================================================================
%%% Includes
%%%===================================================================

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("applev.hrl").

%%%===================================================================
%%% Function exports
%%%===================================================================

-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

%%%===================================================================
%%% local Defines
%%%===================================================================

-define(IN_APP_RESP_MSG_OK,
  "{\"receipt\":{\"original_purchase_date_pst\":\"2012-04-18 05:07:13 America/Los_Angeles\", \"purchase_date_ms\":\"1334750833000\", \"original_transaction_id\":\"1000000042928567\", \"original_purchase_date_ms\":\"1334750833000\", \"transaction_id\":\"1000000042928567\", \"quantity\":\"1\", \"bvrs\":\"1.1.1\", \"hosted_iap_version\":\"1.0.1\", \"product_id\":\"com.appletest.animalshelter.currencyPack1\", \"purchase_date\":\"2012-04-18 12:07:13 Etc/GMT\", \"original_purchase_date\":\"2012-04-18 12:07:13 Etc/GMT\", \"purchase_date_pst\":\"2012-04-18 05:07:13 America/Los_Angeles\", \"bid\":\"com.appletest.animalshelter\", \"item_id\":\"470218188\"}, \"status\":0}").

-define(IN_APP_RESP_MSG_EXPIRED,
  "{\"receipt\":{\"original_purchase_date_pst\":\"2012-04-18 05:07:13 America/Los_Angeles\", \"purchase_date_ms\":\"1334750833000\", \"original_transaction_id\":\"1000000042928567\", \"original_purchase_date_ms\":\"1334750833000\", \"transaction_id\":\"1000000042928567\", \"quantity\":\"1\", \"bvrs\":\"1.1.1\", \"hosted_iap_version\":\"1.0.1\", \"product_id\":\"com.appletest.animalshelter.currencyPack1\", \"purchase_date\":\"2012-04-18 12:07:13 Etc/GMT\", \"original_purchase_date\":\"2012-04-18 12:07:13 Etc/GMT\", \"purchase_date_pst\":\"2012-04-18 05:07:13 America/Los_Angeles\", \"bid\":\"com.appletest.animalshelter\", \"item_id\":\"470218188\"},\"status\":21006}").

-define(IN_APP_RESP_INVALID_JSON,
  "{\"receipt:7898789\"").

-define(APPLE_VALIDATED_RECEIPT,
    #{<<"bid">> => <<"com.appletest.animalshelter">>,
      <<"bvrs">> => <<"1.1.1">>,
      <<"hosted_iap_version">> => <<"1.0.1">>,
      <<"item_id">> => <<"470218188">>,
      <<"original_purchase_date">> =>
          <<"2012-04-18 12:07:13 Etc/GMT">>,
      <<"original_purchase_date_ms">> => <<"1334750833000">>,
      <<"original_purchase_date_pst">> =>
          <<"2012-04-18 05:07:13 America/Los_Angeles">>,
      <<"original_transaction_id">> => <<"1000000042928567">>,
      <<"product_id">> =>
          <<"com.appletest.animalshelter.currencyPack1">>,
      <<"purchase_date">> => <<"2012-04-18 12:07:13 Etc/GMT">>,
      <<"purchase_date_ms">> => <<"1334750833000">>,
      <<"purchase_date_pst">> =>
          <<"2012-04-18 05:07:13 America/Los_Angeles">>,
      <<"quantity">> => <<"1">>,
      <<"transaction_id">> => <<"1000000042928567">>} ).

-define(IN_APP_RESP_MSG_ERR(CODE),
  "{\"status\":" ++ erlang:integer_to_list(CODE) ++ "}").

%% Definition for Default number of messages to test
-define(DEFAULT_MSGS_TO_TEST, 1000).

%% Timeout to wait for all msgs to be received
-define(TIMEOUT_TO_RECEIVE_MSG, 1000).

%%%===================================================================
%%% Test exports
%%%===================================================================
-export([validate_start_stop_ok/1,
         validate_sync_ok/1,
         validate_async_test_ok/1,
         validate_async_test_pid_receipt_ok/1,
         validate_async_redirect_sandbox_ok/1,
         validate_async_expired_ok/1,
         validate_async_server_err_ok/1,
         validate_async_http_invalid_ok/1,
         validate_async_msg_send_apple_timeout_ok/1,
         validate_async_msg_server_err_error/1,
         validate_async_invalid_json_ok/1,
         applev_receipt_full_coverage_ok/1,
         applev_receipt_consumer_cast_full_coverage_ok/1,
         validate_sync_timeout_error/1]).

all() -> [validate_start_stop_ok,
          validate_sync_ok,
          validate_async_test_ok,
          validate_async_test_pid_receipt_ok,
          validate_async_redirect_sandbox_ok,
          validate_async_expired_ok,
          validate_async_server_err_ok,
          validate_async_http_invalid_ok,
          validate_async_msg_send_apple_timeout_ok,
          validate_async_msg_server_err_error,
          validate_async_invalid_json_ok,
          applev_receipt_full_coverage_ok,
          applev_receipt_consumer_cast_full_coverage_ok,
          validate_sync_timeout_error
         ].

%%%===================================================================
%%% init_per_suite:  Contains common initializations for all test
%%%                  cases in the suite
%%%===================================================================
init_per_suite(Config) ->
  Config.

%%%===================================================================
%%% end_per_suite: It is called as the final stage of the test suite
%%%                execution
%%%===================================================================
end_per_suite(_Config) ->
 ok.

%%%===================================================================
%%% init_per_testcase: It is called before each test case in the suite.
%%%                    Contains initialization that must be done for
%%%                    each test case.
%%%
%%% @param Name    Name of the test case
%%% @param Config  Config key-value list of runtime configuration data,
%%%                which has the same value as the list returned by
%%%                init_per_suite
%%%===================================================================
init_per_testcase(_, Config) ->
  %% Configure default mecks
  meck:expect(httpc, request,
    fun(post,_,[], []) ->
           {ok, {?HTTP_OK,0,?IN_APP_RESP_MSG_OK} }
    end),

  %% Reset expectations
  meck:reset(httpc),
  Config.

%%%===================================================================
%%% end_per_testcase: It is called after each test case has finished,
%%%                   enabling cleanup after init_per_testcase
%%%===================================================================
end_per_testcase(_, _Config) ->
  meck:unload(),
  ok.

%%%===================================================================
%%%           Test case functions: Waiting for OK result
%%%===================================================================

%%%===================================================================
%%% Function: validate_syncvalidate_start_stop_ok_ok
%%%
%%% Description: Test the start/stop of the application
%%%===================================================================
validate_start_stop_ok(_Config) ->

  %% Start the Server
  application:ensure_all_started(applev),

  %% Check the server is still running
  ?assertNotEqual( undefined, try_get_state(applev_receipt_sup) ),

  %% Stop Server
  application:stop(applev),

  %% Check the server is not running anymore
  ?assertMatch( undefined, try_get_state(applev_receipt_sup) ),

  ok.

%%%===================================================================
%%% Function: validate_sync_ok
%%%
%%% Description: Test the example validation
%%%===================================================================
validate_sync_ok(_Config) ->
  %% Start the application
  application:ensure_all_started(applev),

  %% Check read OK
  ?assertEqual( {ok, ?APPLE_VALIDATED_RECEIPT}, applev:validate() ),
  ok.

%%%===================================================================
%%% Function: validate_async_test_ok
%%%
%%% Description: Test the async example validation
%%%===================================================================
validate_async_test_ok(_Config) ->
  %% Start the process
  applev_sup:start_link(),

  %% Check read OK
  ?assertMatch( {ok, _}, applev:validate_async() ),

  Res = receive
        ?APPLEV_MSG(ok,R) -> {ok, R}
      after 1000 ->
          error
      end,

  ?assertMatch({ok, _}, Res),
  ok.

%%%===================================================================
%%% Function: validate_async_test_pid_receipt_ok
%%%
%%% Description: Test the async example validation passing parameters
%%%===================================================================
validate_async_test_pid_receipt_ok(_Config) ->
  %% Start the process
  applev_sup:start_link(),

  %% Check read OK
  ?assertMatch( {ok, _}, applev:validate_async(self(), ?APPLE_SANDBOX_RECEIPT)),

  Res = receive
        ?APPLEV_MSG(ok,R) -> {ok, R}
      after 1000 ->
          error
      end,

  ?assertMatch({ok, _}, Res),
  ok.


%%%===================================================================
%%% Function: validate_sync_redirect_sandbox_ok
%%%
%%% Description: This function is going to test if the receipt was created
%%%              for sandbox and sent for prodution. In this case, the receipt
%%%              is redirect sandbox that will return OK
%%%===================================================================
validate_async_redirect_sandbox_ok(_Config) ->
  %% Start the process
  applev_sup:start_link(),

  %% If request is for SandBox return OK, if production, redirect
  meck:expect(httpc, request,
    fun(_, Req, _, _) ->
      case Req of
        %% If request is for Sandbox return OK
        {?APPLE_SAND_BOX,_, ?APPLE_CONTENT_TYPE, _} ->
          {ok, {?HTTP_OK,0,?IN_APP_RESP_MSG_OK} };
        %% If request is for Production redirect to sandbox
        {?APPLE_PRODUCTION,_, ?APPLE_CONTENT_TYPE, _} ->
          {ok, {?HTTP_OK,0, ?IN_APP_RESP_MSG_ERR(?APPLE_STATUS_SANDBOX)} }
      end
    end),

  %% Create all servers that will process the validation
  create_receipt_validation_server(self(), ?DEFAULT_MSGS_TO_TEST),

  %% Wait all messages to be processed
  Result = message_server_wait(?DEFAULT_MSGS_TO_TEST, ?TIMEOUT_TO_RECEIVE_MSG),

  ?assertEqual( ?DEFAULT_MSGS_TO_TEST, check_received_msgs(ok, Result) ),

  %% Check there were twice the number of https request
  ?assertEqual( 2*?DEFAULT_MSGS_TO_TEST,
                meck:num_calls(httpc, request, [post,'_','_', '_']) ),
  ok.


%%%===================================================================
%%% Function: validate_async_expired_ok
%%%
%%% Description: This function is going to test if the receipt was expired
%%%              In this case, the post queue will receive an invalid receipt
%%%===================================================================
validate_async_expired_ok(_Config) ->
  %% Start the process
  applev_sup:start_link(),

  %% Force apple answer to be valid but expired
  meck:expect(httpc, request,
    fun(_, _, _, _) ->
      {ok, {?HTTP_OK,0, ?IN_APP_RESP_MSG_EXPIRED} }
    end),

  %% Create all servers that will process the validation
  create_receipt_validation_server(self(), ?DEFAULT_MSGS_TO_TEST),

  %% Wait all messages to be processed
  Result = message_server_wait(?DEFAULT_MSGS_TO_TEST, ?TIMEOUT_TO_RECEIVE_MSG),

  ?assertEqual( ?DEFAULT_MSGS_TO_TEST,
                       check_received_msgs({error, receipt_expired}, Result) ),

  %% Check the number of http request
  ?assertEqual( ?DEFAULT_MSGS_TO_TEST,
                meck:num_calls(httpc, request, [post,'_','_', '_']) ),
  ok.


%%%===================================================================
%%% Function: validate_async_server_err_ok
%%%
%%% Description: This function is going to test what happens when apple
%%%              server returns an error to retry, the gen_server must
%%%              retry and succeed.
%%%===================================================================
validate_async_server_err_ok(_Config) ->
  %% Start the process
  applev_sup:start_link(),

  %% Configure to return error at the first request
  meck:expect(httpc, request,
    fun(_, _, _, _) ->
      case get({number_of_calls}) of
        undefined -> % Return error at the first request
          put({number_of_calls}, 1),
          {ok, {?HTTP_OK,0, ?IN_APP_RESP_MSG_ERR(?APPLE_STATUS_ERR_SERVER_1)} };
        _ -> % return OK for all other requests
          {ok, {?HTTP_OK,0, ?IN_APP_RESP_MSG_OK} }
      end
    end),

  %% Create all servers that will process the validation
  create_receipt_validation_server(self(), ?DEFAULT_MSGS_TO_TEST),

  %% Wait all messages to be processed
  Result = message_server_wait(?DEFAULT_MSGS_TO_TEST, ?TIMEOUT_TO_RECEIVE_MSG),

  ?assertEqual( ?DEFAULT_MSGS_TO_TEST, check_received_msgs(ok, Result) ),

  %% Check there were twice the number of https request
  ?assertEqual( 2*?DEFAULT_MSGS_TO_TEST,
                meck:num_calls(httpc, request, [post,'_','_', '_']) ),
  ok.

%%%===================================================================
%%% Function: validate_async_http_invalid_ok
%%%
%%% Description: This function is going to test what happens when the
%%%              server receives an http error when validating Apple receipt
%%%              It is expected to try again.
%%%===================================================================
validate_async_http_invalid_ok(_Config) ->
  %% Start the process
  applev_sup:start_link(),

  %% Configure to return an error at first request
  meck:expect(httpc, request,
    fun(_, _, _, _) ->
      case get({number_of_calls}) of
        undefined -> % Return error at the first request
          put({number_of_calls}, 1),
          error;
        _ -> % return OK for all other requests
          {ok, {?HTTP_OK,0, ?IN_APP_RESP_MSG_OK} }
      end
   end),

  %% Create all servers that will process the validation
  create_receipt_validation_server(self(), ?DEFAULT_MSGS_TO_TEST),

  %% Wait all messages to be processed
  Result = message_server_wait(?DEFAULT_MSGS_TO_TEST, ?TIMEOUT_TO_RECEIVE_MSG),

  ?assertEqual( ?DEFAULT_MSGS_TO_TEST, check_received_msgs(ok, Result) ),

  %% Check the httpc request was called twice
  ?assertEqual(2*?DEFAULT_MSGS_TO_TEST,
                          meck:num_calls(httpc, request, ['_', '_', '_', '_'])),
  ok.


%%%===================================================================
%%% Function: validate_async_msg_send_apple_timeout_ok
%%%
%%% Description: This function is testing the maximum number of tries
%%%              for apple website.
%%%===================================================================
validate_async_msg_send_apple_timeout_ok(_Config) ->
  %% Start the process
  applev_sup:start_link(),

  %% Configure to return always OK
  meck:expect(httpc, request,
    fun(_, _, _, _) ->
      {ok, {?HTTP_OK,0, ?IN_APP_RESP_MSG_ERR(?APPLE_STATUS_ERR_SERVER_1)} }
    end),

  %% Create all servers that will process the validation
  create_receipt_validation_server(self(), ?DEFAULT_MSGS_TO_TEST),

  %% Wait all messages to be processed
  Result = message_server_wait(?DEFAULT_MSGS_TO_TEST, ?TIMEOUT_TO_RECEIVE_MSG),

  ?assertEqual( ?DEFAULT_MSGS_TO_TEST, check_received_msgs({error, max_number_of_tries}, Result) ),

  %% Check the httpc request by the number of tries
  ?assertEqual( 3*?DEFAULT_MSGS_TO_TEST,
                meck:num_calls(httpc, request, [post,'_','_', '_']) ),
  ok.

%%%===================================================================
%%% Function: validate_async_msg_server_err_error
%%%
%%% Description: This function is going to test what happens when apple
%%%              server returns an invalid receipt, it should be discarded
%%%              because there is no transaction_id associated with.
%%%              No messages should be sent to post queue
%%%===================================================================
validate_async_msg_server_err_error(_Config) ->
  %% Start the process
  applev_sup:start_link(),

  %% Configure to return error at the first request
  meck:expect(httpc, request,
    fun(_, _, _, _) ->
      {ok, {?HTTP_OK,0, ?IN_APP_RESP_MSG_ERR(?APPLE_STATUS_ERR_CORRUPTED)} }
    end),

  %% Create all servers that will process the validation
  create_receipt_validation_server(self(), ?DEFAULT_MSGS_TO_TEST),

  %% Wait all messages to be processed
  Result = message_server_wait(?DEFAULT_MSGS_TO_TEST, ?TIMEOUT_TO_RECEIVE_MSG),

  ?assertEqual( ?DEFAULT_MSGS_TO_TEST, check_received_msgs({error, receipt_corrupted}, Result) ),
  ok.

%%%===================================================================
%%% Function: validate_async_invalid_json_ok
%%%
%%% Description: This function is going to test what happens when the
%%%              server receives an http message with an invalid json
%%%              It is expected to try again.
%%%===================================================================
validate_async_invalid_json_ok(_Config) ->
  %% Start the process
  applev_sup:start_link(),

  %% Configure to return an error at first request
  meck:expect(httpc, request,
    fun(_, _, _, _) ->
      case get({number_of_calls}) of
        undefined -> % Return error at the first request
          put({number_of_calls}, 1),
          {ok, {?HTTP_OK,0, ?IN_APP_RESP_INVALID_JSON} };
        _ -> % return OK for all other requests
          {ok, {?HTTP_OK,0, ?IN_APP_RESP_MSG_OK} }
      end
   end),

  %% Create all servers that will process the validation
  create_receipt_validation_server(self(), ?DEFAULT_MSGS_TO_TEST),

  %% Wait all messages to be processed
  Result = message_server_wait(?DEFAULT_MSGS_TO_TEST, ?TIMEOUT_TO_RECEIVE_MSG),

  ?assertEqual( ?DEFAULT_MSGS_TO_TEST, check_received_msgs(ok, Result) ),

  %% Check the httpc request was called twice
  ?assertEqual(2*?DEFAULT_MSGS_TO_TEST,
                          meck:num_calls(httpc, request, ['_', '_', '_', '_'])),
  ok.

%%%===================================================================
%%% Function: applev_receipt_full_coverage_ok
%%%
%%% Description: This test will only guarantee 100% coverage for
%%%              applev_receipt.erl
%%%===================================================================
applev_receipt_full_coverage_ok(_Config) ->
  %% Start the process
  applev_sup:start_link(),

  %% Configure to return always OK
  meck:expect(httpc, request,
    fun(_, _, _, _) ->
      {ok, {?HTTP_OK,0, ?IN_APP_RESP_INVALID_JSON} }
    end),

  {ok, Pid} = applev_receipt_sup:process_msg(self(), ?APPLE_SANDBOX_RECEIPT),

  gen_server:cast(Pid, {none}),
  gen_server:call(Pid, {none}),
  erlang:send(Pid, {none}),
  applev_receipt:code_change(none, none, none),

  ok.

%%%===================================================================
%%% Function: applev_receipt_consumer_cast_full_coverage_ok
%%%
%%% Description: This test will only guarantee 100% coverage for
%%%              applev_receipt_consumer.erl
%%%===================================================================
applev_receipt_consumer_cast_full_coverage_ok(_Config) ->
  %% Start the process
  applev_sup:start_link(),

  {ok, PidDest } = applev_receipt_consumer:start_link(),
  applev_receipt_consumer:code_change(none, none, none),
  gen_server:cast(PidDest, {none}),

  %% Check the server is not running anymore
  ?assertMatch( undefined, try_get_state(PidDest) ),
  ok.

%%%===================================================================
%%% Function: validate_sync_timeout_error
%%%
%%% Description: This test will check if we get a timeout for not
%%%              receiving the result in an expected time
%%%===================================================================
validate_sync_timeout_error(_Config) ->
  %% Start the process
  applev_sup:start_link(),

  { ok, PidDest } = applev_receipt_consumer:start_link(100),
  timer:sleep(500),

  %% Check the server is not running anymore
  ?assertMatch( undefined, try_get_state(PidDest) ),
  ok.

%%%===================================================================
%%% local functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc This function creates the required number of validation server
%%      with default sandbox message
%%
%% @param Pid Destination Pid
%% @param NumberOfServer Number of entities
%% @end
%%--------------------------------------------------------------------
create_receipt_validation_server(Pid, NumberOfServer) ->
  lists:foreach(
    fun(_) ->
      applev_receipt_sup:process_msg(Pid, ?APPLE_SANDBOX_RECEIPT)
    end,
    lists:seq(1,NumberOfServer)
  ).

message_server_wait(Messages, Timeout) ->
  lists:map(
    fun(_) ->
      receive
        ?APPLEV_MSG(ok,R) ->    {ok, R};
        ?APPLEV_MSG(error,R) -> {{error, R}, not_used}
      after Timeout ->
          {{timeout}, not_used}
      end
    end,
    lists:seq(1,Messages)
  ).

check_received_msgs(ExpectedResult, List) ->
  FilteredList = proplists:get_all_values(ExpectedResult, List),
  length(FilteredList).

%%--------------------------------------------------------------------
%% @doc This function try to get the state of a registered server
%%
%% @param Name Server name
%% @end
%%--------------------------------------------------------------------
try_get_state(Name)->
  try sys:get_state(Name) of
    S -> S
  catch
    _:_ -> undefined
  end.





