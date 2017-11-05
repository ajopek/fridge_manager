%%%%---------------------------------------------------------------------------
%%%% @author Artur Jopek
%%%%---------------------------------------------------------------------------
%%%% @doc
%%%% This module provides ulity functions for REST endpoint, built upon
%%%% manager_server api. 
%%%% @end
%%%%---------------------------------------------------------------------------

-module(fridge_manager_utils).
-export([check_recipe/2, execute_recipe/1, withdraw_recipe/1, insert/2, withdraw/2]).
-import(manager_server,
        [check_avalibility/2, insert_product/2, withdraw_product/2]).

%% ----------------------------------------------------------------------------
%% Avaliable from rest endpoit
%% ----------------------------------------------------------------------------

%% @doc
%% Withdraw by name and quantity in binary
%% @end
-spec withdraw(binary(), binary()) ->
  ok | not_enough.
withdraw(BinName, BinQuantity) ->
  Name = list_to_atom(binary_to_list(BinName)),
  Quantity = list_to_integer(binary_to_list(BinQuantity)),
  case manager_server:check_avalibility(Name, Quantity) of
    {missing, _} -> not_enough;
  ok           ->
    manager_server:withdraw_product(Name, Quantity),
    ok
  end.

%% @doc
%% Insert by name and quantity in binary
%% @end
-spec insert(binary(), binary()) ->
  ok.
insert(BinName, BinQuantity)  ->
  Name = list_to_atom(binary_to_list(BinName)),
  Quantity = list_to_integer(binary_to_list(BinQuantity)),
  manager_server:insert_product(Name, Quantity).

%% ----------------------------------------------------------------------------
%% Not avaliable from rest endpoit
%% ----------------------------------------------------------------------------

%% @doc
%% Check avaliability of products for a recipe
%% @end
-spec(check_recipe(list(), list()) -> ok | {missing, list()}).
check_recipe([{Name, Quantity} | T], Missing) ->
  case check_avalibility(Name, Quantity) of
    ok                    -> check_recipe(T, Missing);
    {missing, _} = Result ->
      check_recipe(T, [Result | Missing])
  end;

check_recipe([], []) ->
  ok;

check_recipe([], Missing) ->
  {missing, Missing}.
%% @doc
%% Check avaliability and withdraw products for a recipe
%% Returns list of missing products if execution not possible
%% @end
-spec(execute_recipe([{atom(), integer()}]) -> ok | {missing, list()}).
execute_recipe(Recipe) ->
  case check_recipe(Recipe, []) of
    ok                    -> withdraw_recipe(Recipe);
    {missing, _} = Result -> Result
  end.

%% @doc
%% Withdraw products accordingly to list
%% @end
-spec(withdraw_recipe(list()) -> ok | {error, not_enough}).
withdraw_recipe([{Name, Quantity} | T]) ->
  case withdraw_product(Name, Quantity) of
    true  -> withdraw_recipe(T);
    false -> {error, not_enough}
  end;
withdraw_recipe([]) ->
  ok.
