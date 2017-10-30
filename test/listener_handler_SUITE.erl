-module(listener_handler_SUITE).
% common test
-include_lib("common_test/include/ct.hrl").

% etest macros
-include_lib("etest/include/etest.hrl").
% etest_http macros
-include_lib("etest_http/include/etest_http.hrl").

-export([init_per_suite/1, end_per_suite/1, end_per_testcase/2,
        init_per_testcase/2, all/0]).
-export([get_with_good_args_test/1,
         get_with_bad_args_test/1,
         post_test/1,
         bad_content_type_test/1]).
% Invoked before all tests
init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(cowboy),
  {ok, _} = fridge_manager_listener:start(),
  Config.

% Invoked after all tests
end_per_suite(_Config) ->
  application:stop(cowboy),
  application:stop(ranch),
  ok.

% Invoked before each tests
init_per_testcase(_TestCase, Config) ->
    meck:new(fridge_manager_utils),
    Config.

% Invoked after each tests
end_per_testcase(_TestCase, _Config) ->
    meck:unload(fridge_manager_utils),
    ok.

% Returns list of test cases
all() ->
  [get_with_good_args_test,
   get_with_bad_args_test,
   post_test,
   bad_content_type_test].


get_with_good_args_test(_Config) ->
  meck:expect(fridge_manager_utils, withdraw, fun(_, _) -> ok end),
  Response = ?perform_get("http://localhost:8080/test/test/5"),
  ?assert_status(200, Response),
  ?assert_body("Withdrawn", Response),
  ok.

get_with_bad_args_test(_Config)  ->
  meck:expect(fridge_manager_utils, withdraw, fun(_, _) -> not_enough end),
  Response = ?perform_get("http://localhost:8080/test/test/5"),
  ?assert_status(200, Response),
  ?assert_body("Not enough", Response),
  ok.

post_test(_Config)               ->
  meck:expect(fridge_manager_utils, insert, fun(_, _) -> ok end),
  Response = ?perform_post("http://localhost:8080/test/test/5",
                           [{"content-type", <<"text/plain">>}]),
  ?assert_status(200, Response),
  ?assert_body("Added", Response),
  ok.

bad_content_type_test(_Config)   ->
  Response = ?perform_post("http://localhost:8080/test/test/5",
                           [{"content-type", <<"text/html">>}]),
  ?assert_status(415, Response),
  ok.
