%%%%-------------------------------------------------------------------
%%%% @author Artur Jopek
%%%%-------------------------------------------------------------------
%%%% @doc
%%%% This application is a fridge manager, it consists of 4 main modules:
%%%%   - manager_server - gen_server which manages in fridge state and provides
%%%%     basic API
%%%%   - fridge_manager_utils - provides utility functions built upon
%%%%     manager_server API
%%%%   - fridge_manager_listener - cowboy http listener
%%%%   - listener_handler - callbacks implementation for http listener, defines
%%%%     rest endpoint
%%%% @end
%%%%-------------------------------------------------------------------

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
