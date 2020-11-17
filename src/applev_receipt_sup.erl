%%%-------------------------------------------------------------------
%%% Created : 29 Oct 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc This is the supervisor that will supervise the message validation
%%% @end
%%%-------------------------------------------------------------------

-module(applev_receipt_sup).

-author('Thiago Esteves').

%%====================================================================
%% API functions
%%====================================================================

-export([start_link/0]).

-export([init/1, process_msg/3]).

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

-define(SERVER,           ?MODULE).
-define(MSG_CONSUMER_NAME, applev_receipt).

%%====================================================================
%% API functions implementation
%%====================================================================
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec init(list()) -> {ok, tuple()} | {error, term()}.
init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 5,
                 period => 30},
    %% The child won't start automatically
    ChildSpecs = #{ id => ?MSG_CONSUMER_NAME,
                    start => {?MSG_CONSUMER_NAME, start_link, []},
                    restart => transient,
                    shutdown => brutal_kill,
                    type => worker },
    {ok, {SupFlags, [ChildSpecs]}}.

%%--------------------------------------------------------------------
%% @doc This function creates a supervised gen_server to validate
%%      the received message and in case of any failure, the supervisor
%%      will recriate a newone with the same message
%%
%% @param PidDest Pid or Atom that will validated receipt
%% @param ReceiptToValidate Receit to be validated
%% @param Args Any argument or metadata related to the receipt
%% @end
%%--------------------------------------------------------------------
-spec process_msg(pid() | atom(), binary(), any()) -> { ok , undefined | pid() }.
process_msg(PidDest, ReceiptToValidate, Args) ->
  {ok, Pid} = supervisor:start_child(?MODULE, [PidDest, ReceiptToValidate, Args]),
  {ok, Pid}.

%%====================================================================
%% Internal functions
%%====================================================================