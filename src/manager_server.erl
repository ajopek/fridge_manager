%%%%---------------------------------
%%%% @author Artur Jopek
%%%%---------------------------------
%%%% @doc
%%%% This module is responsible for handling status in fridge.
%%%% @end
%%%%---------------------------------

-module(manager_server).
-behaviour(gen_server).

%% api
-export([start/0, check_avalibility/2, insert_product/2, withdraw_product/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2, stop/0]).


%%-----------
%% Api
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

%% @doc
%% Starts the manager server.
%% @end
-spec start() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @private @doc
%% Initializes the server.
%% @end
-spec init([]) ->
  {ok, #{}} |
  {stop, Reason :: term()} | ignore.
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
  {ok, NewInFridge} = add_quantity(Name, Quantity, InFridge),
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
  case subtract_quantity(Name, Quantity, InFridge) of
    {ok, ProcessedInFridge} ->
      {true, ProcessedInFridge};
    {error, _} ->
      {false, InFridge}
  end,
    {reply, Reply, NewInFridge};

%% @doc
%% Check if enough quantity is in the fridge
%% @end
handle_call({is_enough, Name, Quantity}, _From, InFridge) ->
  Answer =
    case find_quantity(Name, InFridge) of
      {ok, AvaliableQuantity}
      when AvaliableQuantity >= Quantity ->
        ok;
      {ok, AvaliableQuantity}            ->
        {missing, {Name, AvaliableQuantity - Quantity}};
      {error, no_product}                ->
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
%% Standard terminate callback.
%% @end
-spec terminate(term(), map())     ->
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
%% Updates quantity of product by name.
%% @end
-spec update_quantity(atom(), integer(), map()) ->
  {ok, map()}.
update_quantity(Name, NewQuantity, InFridge) ->
  NewInFridge = maps:update(Name, NewQuantity, InFridge),
  {ok, NewInFridge}.

%% @doc
%% Finds quantity of product by name.
%% @end
-spec find_quantity(atom(), map()) ->
  {ok, integer()} |
  {error, atom()}.
find_quantity(Name, InFridge) ->
  case maps:find(Name, InFridge) of
    {ok, Quantity} ->
      {ok, Quantity};
    error          ->
      {error, no_product}
  end.

%% @doc
%% Checks wheather the product exists, then adds to quantity.
%% @end
-spec add_quantity(atom(), integer(), map()) ->
  {ok, map()}.
add_quantity(Name, QuantityToAdd, InFridge) ->
  case find_quantity(Name, InFridge) of
    {ok, OldQuantity}   ->
      NewQuantity = OldQuantity + QuantityToAdd,
      update_quantity(Name, NewQuantity, InFridge);
    {error, no_product} ->
      add_product(Name, QuantityToAdd, InFridge)
  end.

%% @doc
%% Adds new product.
%% @end
-spec add_product(atom(), integer(), map()) ->
  {ok, map()}.
add_product(Name, Quantity, InFridge) ->
  NewInFridge = maps:put(Name, Quantity, InFridge),
  {ok, NewInFridge}.

%% @doc
%% Subtracts quantity of product by name.
%% @end
-spec subtract_quantity(atom(), integer(), map()) ->
  {ok, map()} |
  {error, {missing, {atom(), integer()}}}.
subtract_quantity(Name, Quantity, InFridge) ->
  case find_quantity(Name, InFridge) of
    {ok, AvaliableQuantity}
      when AvaliableQuantity >= Quantity ->
        NewQuantity = AvaliableQuantity - Quantity,
        {ok, NewInFridge} = update_quantity(Name, NewQuantity, InFridge);
    {ok, AvaliableQuantity} ->
        MissingQuantity = AvaliableQuantity - Quantity,
      {error, {missing, {Name, MissingQuantity}}};
    {error, no_product}                  ->
      {error, {missing, {Name, Quantity}}}
  end.
