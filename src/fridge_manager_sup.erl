%%%%---------------------------------------------------------------------------
%%%% @author Artur Jopek
%%%%---------------------------------------------------------------------------
%%%% @doc
%%%% Fridge manager app top level supervisor, it starts manager_server and
%%%% cowboy TCP listener.
%%%% @end
%%%%---------------------------------------------------------------------------

-module(fridge_manager_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
-spec init(Args :: term()) ->
  {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    ChildrenSpecs = [
      { server,
        {manager_server, start, []},
        permanent,
        10000,
        worker,
        [manager_server]},
      { listener,
        {fridge_manager_listener, start, []},
        permanent,
        10000,
        worker,
        [fridge_manager_listener]}
    ],
    {ok, { {one_for_one, 1000, 3600}, ChildrenSpecs} }.

%%====================================================================
%% Internal functions
%%====================================================================
