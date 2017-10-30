%%%-------------------------------------------------------------------
%% @doc fridge_manager public API
%% @end
%%%-------------------------------------------------------------------

-module(fridge_manager_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    fridge_manager_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
