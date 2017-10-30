-module(listener_handler).

-export([init/2]).
%% Rest callbacks
-export([content_types_provided/2, allowed_methods/2, to_text/2,
				 content_types_accepted/2, handle_text_content/2]).

-type req()    :: cowboy_req:req().
-type state()  :: any().

%% @doc
%% Initialize handler.
%% Returns {cowboy_rest, Req, State} to switch to cowboy_rest.
%% @end
-spec init(req(), state()) ->
	{cowboy_rest, req(), state()}.
init(Req, State) ->
	{cowboy_rest, Req, State}.

%% @doc
%% Specifies allowed http methods.
%% @end
-spec allowed_methods(req(), state()) ->
	{[binary()], req(), state()}.
allowed_methods(Req, State) ->
	AllowedMethods = [<<"GET">>, <<"POST">>],
	{AllowedMethods, Req, State}.

%% @doc
%% Specifies accepted conttent types and callbacks,
%% for each type, for POST method.
%% @end
-spec content_types_accepted(req(), state()) ->
	{[{binary(), atom()}],req(), state()}.
content_types_accepted(Req, State) ->
	AcceptedTypes = [{<<"text/plain">>, handle_text_content}],
	{AcceptedTypes, Req, State}.

%% @doc
%% Process POST.
%% @end
-spec handle_text_content(req(), state()) ->
	{true, req(), state()}.
handle_text_content(Req, State) ->
	#{name := Name, quantity := Quantity} = cowboy_req:bindings(Req),
	ok = fridge_manager_utils:insert(Name, Quantity),
	Req1 = cowboy_req:set_resp_body(<<"Added">>, Req),
	{true , Req1, State}.

%% @doc
%%	Content types provided for GET request.
%% @end
-spec content_types_provided(req(), state()) ->
	{[{binary(), atom()}], req(), state()}.
content_types_provided(Req, State) ->
	AcceptedTypes = [{<<"text/plain">>, to_text}],
	{AcceptedTypes, Req, State}.

%% @doc
%% Process GET.
%% @end
-spec to_text(req(), state()) ->
	{binary(), req(), state()}.
to_text(Req, State) ->
	#{name := Name, quantity := Quantity} = cowboy_req:bindings(Req),
	Reply =
		case fridge_manager_utils:withdraw(Name, Quantity) of
			not_enough ->
				<<"Not enough">>;
			ok ->
				<<"Withdrawn">>
		end,
  {Reply, Req, State}.
