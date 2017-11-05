%%%%---------------------------------------------------------------------------
%%%% @author Artur Jopek
%%%%---------------------------------------------------------------------------
%%%% @doc
%%%% This module manages cowboy TCP listener. 
%%%% @end
%%%%---------------------------------------------------------------------------
-module(fridge_manager_listener).
-export([start/0, stop/1]).

%% @doc
%% Starts clear TCP listener on port 8080.
%% All paths handeled by listener_handler.
%% @end
-spec start() ->
	 {ok, ListenerPid :: pid()}
	 | {error, any()}.
start() ->
	Dispatch = cowboy_router:compile([
		{'_', [{"/test/[:name]/[:quantity]", listener_handler,[]}]}
	]),
	{ok, _} = cowboy:start_clear(?MODULE, [{port, 8080}],
										 #{env => #{dispatch => Dispatch}}).
-spec stop(term()) ->
	ok.
stop(_State) ->
	ok.
