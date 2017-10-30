-module(manager_server_test).
-include_lib("eunit/include/eunit.hrl").
-export([functionality_test_/0, start/0, stop/1, add_product/0,
         check_avalibility/0, withdraw_product/0, try_witdrawing_nonexisting/0,
         try_witdrawing_more/0]).

-define(SERVER, manager_server).

functionality_test_() ->
  {foreach,
    fun start/0,
    fun stop/1,
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

start()                      ->
  {ok, Pid} = manager_server:start(),
  Pid.

stop(_)                      ->
  manager_server:stop().

add_product()                ->
  Result = gen_server:cast(?SERVER, {bought, test, 10}),
  ?_assertEqual(ok, Result).

check_avalibility()          ->
  gen_server:cast(?SERVER, {bought, test, 10}),
  Result0 = gen_server:call(?SERVER,{is_enough, test, 10}),
  ?_assertEqual(true ,Result0),
  Result1 = gen_server:call(?SERVER,{is_enough, test, 12}),
  ?_assertEqual(false ,Result1).

withdraw_product()           ->
  gen_server:cast(?SERVER, {bought, test, 10}),
  Result0 = gen_server:call(?SERVER,{consume, test, 9}),
  ?_assertEqual(true ,Result0),
  Result1 = gen_server:call(?SERVER,{consume, test, 1}),
  ?_assertEqual(true ,Result1).

try_witdrawing_nonexisting() ->
  Result = gen_server:call(?SERVER,{consume, test, 9}),
  ?_assertEqual(false ,Result).

try_witdrawing_more()        ->
  gen_server:cast(?SERVER, {bought, test, 10}),
  Result = gen_server:call(?SERVER,{consume, test, 11}),
  ?_assertEqual(false ,Result).
