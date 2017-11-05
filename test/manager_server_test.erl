%%%%---------------------------------------------------------------------------
%%%% @author Artur Jopek
%%%%---------------------------------------------------------------------------
%%%% @doc
%%%% Manager server unit tests.
%%%% @end
%%%%---------------------------------------------------------------------------
-module(manager_server_test).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
% setup/teardown
-export([setup/0, teardown/1]).
% tests functions
-export([functionality_test_/0, add_product/0, check_avalibility/0,
         withdraw_product/0, try_witdrawing_nonexisting/0,
         try_witdrawing_more/0]).

%%%===================================================================
%%% Tests description
%%%===================================================================

functionality_test_() ->
  {foreach,
    fun setup/0,
    fun teardown/1,
    [
    {"You can add product",
     fun add_product/0},
    {"You can check avalibility",
     fun check_avalibility/0},
    {"You can withdraw product",
     fun withdraw_product/0},
    {"You can't withdraw product that not exist",
      fun try_witdrawing_nonexisting/0
    },
    {"You can't withdraw more than is in fridge",
      fun try_witdrawing_more/0
    }
    ]}.


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================
setup()                      ->
  {ok, Pid} = manager_server:start(),
  Pid.

teardown(_)                      ->
  manager_server:stop().

%%====================================================================
%% Tests functions
%%====================================================================

add_product()                ->
  Result = manager_server:insert_product(test, 10),
  ?_assertEqual(ok, Result).

check_avalibility()          ->
  manager_server:insert_product(test, 10),
  Result0 = manager_server:check_avalibility(test, 10),
  ?_assertEqual(true ,Result0),
  Result1 = manager_server:check_avalibility(test, 12),
  ?_assertEqual(false ,Result1).

withdraw_product()           ->
  Result = manager_server:insert_product(test, 10),
  Result0 = manager_server:withdraw_product(test, 9),
  ?_assertEqual(true ,Result0),
  Result1 = manager_server:withdraw_product(test, 1),
  ?_assertEqual(true ,Result1).

try_witdrawing_nonexisting() ->
  Result = manager_server:withdraw_product(test, 9),
  ?_assertEqual(false ,Result).

try_witdrawing_more()        ->
  manager_server:insert_product(test, 10),
  Result = manager_server:withdraw_product(test, 20),
  ?_assertEqual(false ,Result).

-endif.
