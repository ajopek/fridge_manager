-module(manager_server).
-behaviour(gen_server).
-export([start/0, init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2, stop/0]).
-export([check_avalibility/2, insert_product/2, withdraw_product/2]).

start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, #{}}.

%%-----------
%% Casts
%%-----------

%% @doc
%% Update InFridge status with newly bought products
%% @end
-spec handle_cast({bought, atom(), integer()}, map()) ->
        {noreply, map()};
                ({reset}, map())                     ->
        {noreply, map()}.
handle_cast({bought, Name, Quantity}, InFridge) ->
  NewInFridge =
   case maps:find(Name, InFridge) of
     {ok, OldQuantity} ->
       NewQuantity = OldQuantity + Quantity,
       maps:update(Name, NewQuantity, InFridge);
     error             ->
       maps:put(Name, Quantity, InFridge)
   end,
   {noreply, NewInFridge};
%% @doc
%% Reset InFridge status
%% @end
handle_cast({reset}, _InFridge)                 ->
  {noreply, #{}}.

%%-----------
%% Calls
%%-----------

%% @doc
%% Withdraw from fridge
%% @end
-spec handle_call({consume, atom(), integer()}, {pid(), _}, map())   ->
                   {reply, boolean(), map()};
                 ({is_enough, atom(), integer()}, {pid(), _}, map()) ->
                   {reply, boolean(), map()}.
handle_call({consume, Name, Quantity}, _From, InFridge) ->
  {Reply, NewInFridge} =
  case maps:find(Name, InFridge) of
    {ok, OldQuantity} ->
      NewQuantity = OldQuantity - Quantity,
      {true, maps:update(Name, NewQuantity, InFridge)};
    error             ->
      {false, InFridge}
    end,
    {reply, Reply, NewInFridge};

%% @doc
%% Check if enough quantity is in the fridge
%% @end
handle_call({is_enough, Name, Quantity}, _From, InFridge) ->
  Answer =
    case maps:find(Name, InFridge) of
      {ok, AvaliableQuantity}
      when Quantity =< AvaliableQuantity->
        ok;
      {ok, AvaliableQuantity}           ->
        {missing, {Name, AvaliableQuantity - Quantity}};
      error                             ->
        {missing, {Name, Quantity}}
      end,
  {reply, Answer, InFridge}.


%% @doc
%% Handle any message to gen_server
%% @end
-spec handle_info(any(), map())        ->
  {noreply, map()}.
handle_info(_Info, State)          ->
   {noreply, State}.

%% @doc
%% Standard terminate callback
%% @end
-spec terminate(term(), map())         ->
  ok.
terminate(_Reason, _State)         ->
   ok.

%% @doc
%% Standard code_change callback
%% @end
-spec code_change(any(), map(), any()) ->
  {ok, map()}.
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

%%-----------
%% Utils
%%-----------

%% @doc
%% Check avaliability for a product
%% @end
-spec check_avalibility(atom(), integer()) ->
  ok |
  {missing, {atom(), integer()}}.
check_avalibility(Name, Quantity) ->
  gen_server:call(?MODULE, {is_enough, Name, Quantity}).

%% @doc
%% Insert product
%% @end
-spec insert_product(atom(), integer()) ->
  ok.
insert_product(Name, Quantity) ->
  gen_server:cast(?MODULE, {bought, Name, Quantity}).

%% @doc
%% Withdraw products
%% @end
-spec withdraw_product(atom(), integer()) ->
  boolean().
withdraw_product(Name, Quantity) ->
  gen_server:call(?MODULE,{consume, Name, Quantity}).

%% @doc
%% Stops the manager server.
%% @end
-spec stop() ->
  ok.
stop() ->
  gen_server:stop(?MODULE).
