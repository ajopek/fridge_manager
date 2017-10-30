-module(fridge_manager_SUITE).
% common test
-include_lib("common_test/include/ct.hrl").

% etest macros
-include_lib("etest/include/etest.hrl").
% etest_http macros
-include_lib("etest_http/include/etest_http.hrl").
-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2,
         end_per_testcase/2, all/0]).
-export([low_quantity_withdraw_test/1, unexisting_wtihdraw_test/1,
         put_product_test/1, withdraw_product_test/1]).

% Invoked before all tests
init_per_suite(Config) ->
  application:ensure_all_started(fridge_manager),
  Config.

% Invoked after all tests
end_per_suite(_Config) ->
  application:stop(fridge_manager),
  application:stop(cowboy),
  application:stop(ranch),
  ok.

% Invoked before each tests
init_per_testcase(_TestCase, Config) ->
    Config.

% Invoked after each tests
end_per_testcase(_TestCase, _Config) ->
    gen_server:cast(manager_server, {reset}),
    ok.
% Returns list of test cases
all() ->
  [low_quantity_withdraw_test, unexisting_wtihdraw_test,
  put_product_test, withdraw_product_test].

put_product_test(_Config)          ->
  Response = ?perform_post("http://localhost:8080/test/test/5",
                           [{"content-type", <<"text/plain">>}]),
  ?assert_status(200, Response),
  ?assert_body("Added", Response).

withdraw_product_test(_Config)     ->
  ?perform_post("http://localhost:8080/test/test/5",
                [{"content-type", <<"text/plain">>}]),
  Response = ?perform_get("http://localhost:8080/test/test/5"),
  ?assert_status(200, Response),
  ?assert_body("Withdrawn", Response).

unexisting_wtihdraw_test(_Config) ->
  Response = ?perform_get("http://localhost:8080/test/test/5"),
  ?assert_status(200, Response),
  ?assert_body("Not enough", Response).

low_quantity_withdraw_test(_Config)       ->
  ?perform_post("http://localhost:8080/test/test/3",
                [{"content-type", <<"text/plain">>}]),
  Response = ?perform_get("http://localhost:8080/test/test/5"),
  ?assert_status(200, Response),
  ?assert_body("Not enough", Response).
