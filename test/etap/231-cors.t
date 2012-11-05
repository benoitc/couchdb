#!/usr/bin/env escript
%% -*- erlang -*-

% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-record(user_ctx, {
    name = null,
    roles = [],
    handler
}).


-define(SUPPORTED_METHODS, "GET, HEAD, POST, PUT, DELETE, TRACE, CONNECT, COPY, OPTIONS").
server() ->
    lists:concat([
        "http://127.0.0.1:",
        mochiweb_socket_server:get(couch_httpd, port),
        "/"
    ]).


main(_) ->
    test_util:init_code_path(),

    etap:plan(16),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

dbname() -> "etap-test-db".
dbname1() -> "etap-test-db1".
dbname2() -> "etap-test-db2".

admin_user_ctx() -> {user_ctx, #user_ctx{roles=[<<"_admin">>]}}.

set_admin_password(UserName, Password) ->
    Salt = binary_to_list(couch_uuids:random()),
    Hashed = couch_util:to_hex(crypto:sha(Password ++ Salt)),
    couch_config:set("admins", UserName,
        "-hashed-" ++ Hashed ++ "," ++ Salt, false).

test() ->

    ibrowse:start(),
    crypto:start(),

    %% launch couchdb
    couch_server_sup:start_link(test_util:config_files()),

    %% initialize db
    timer:sleep(1000),
    couch_server:delete(list_to_binary(dbname()), [admin_user_ctx()]),
    couch_server:delete(list_to_binary(dbname1()), [admin_user_ctx()]),
    couch_server:delete(list_to_binary(dbname2()), [admin_user_ctx()]),
    {ok, Db} = couch_db:create(list_to_binary(dbname()), [admin_user_ctx()]),
    {ok, Db1} = couch_db:create(list_to_binary(dbname1()), [admin_user_ctx()]),
    {ok, Db2} = couch_db:create(list_to_binary(dbname2()), [admin_user_ctx()]),

    % CORS is disabled by default
    test_no_headers_server(),
    test_no_headers_db(),

    % Now enable CORS
    ok = couch_config:set("httpd", "enable_cors", "true", false),
    ok = couch_config:set("cors", "origins", "http://example.com", false),

    %% do tests
    test_incorrect_origin_simple_request(),
    test_incorrect_origin_preflight_request(),

    test_preflight_request(),
    test_db_request(),
    test_db_preflight_request(),
    test_db_origin_request(),
    test_db1_origin_request(),
    test_preflight_with_port1(),
    test_preflight_with_scheme1(),

    ok = couch_config:set("cors", "origins", "http://example.com:5984", false),
    test_preflight_with_port2(),

    ok = couch_config:set("cors", "origins", "https://example.com:5984", false),
    test_preflight_with_scheme2(),




    ok = couch_config:set("cors", "origins", "*", false),
    test_preflight_with_wildcard(),

    ok = couch_config:set("cors", "origins", "http://example.com", false),

    %% do tests with auth
    ok = set_admin_password("test", "test"),

    test_db_preflight_auth_request(),
    test_db_origin_auth_request(),


    %% restart boilerplate
    catch couch_db:close(Db),
    catch couch_db:close(Db1),
    catch couch_db:close(Db2),

    couch_server:delete(list_to_binary(dbname()), [admin_user_ctx()]),
    couch_server:delete(list_to_binary(dbname1()), [admin_user_ctx()]),
    couch_server:delete(list_to_binary(dbname2()), [admin_user_ctx()]),

    timer:sleep(3000),
    couch_server_sup:stop(),
    ok.

%% Cors is disabled, should not return Access-Control-Allow-Origin
test_no_headers_server() ->
    Headers = [{"Origin", "http://127.0.0.1"}],
    {ok, _, Resp, _} = ibrowse:send_req(server(), Headers, get, []),
    etap:is(proplists:get_value("Access-Control-Allow-Origin", Resp),
            undefined, "No CORS Headers when disabled").

%% Cors is disabled, should not return Access-Control-Allow-Origin
test_no_headers_db() ->
    Headers = [{"Origin", "http://127.0.0.1"}],
    Url = server() ++ "etap-test-db",
    {ok, _, Resp, _} = ibrowse:send_req(Url, Headers, get, []),
    etap:is(proplists:get_value("Access-Control-Allow-Origin", Resp),
            undefined, "No CORS Headers when disabled").

test_incorrect_origin_simple_request() ->
    Headers = [{"Origin", "http://127.0.0.1"}],
    {ok, _, RespHeaders, _} = ibrowse:send_req(server(), Headers, get, []),
    etap:is(proplists:get_value("Access-Control-Allow-Origin", RespHeaders),
            undefined,
            "Specified invalid origin, no Access").

test_incorrect_origin_preflight_request() ->
    Headers = [{"Origin", "http://127.0.0.1"},
               {"Access-Control-Request-Method", "GET"}],
    {ok, _, RespHeaders, _} = ibrowse:send_req(server(), Headers, options, []),
    etap:is(proplists:get_value("Access-Control-Allow-Origin", RespHeaders),
            undefined,
            "invalid origin").

test_preflight_request() ->
    Headers = [{"Origin", "http://example.com"},
               {"Access-Control-Request-Method", "GET"}],
    case ibrowse:send_req(server(), Headers, options, []) of
    {ok, _, RespHeaders, _}  ->
        etap:is(proplists:get_value("Access-Control-Allow-Methods", RespHeaders),
            ?SUPPORTED_METHODS,
            "test_preflight_request Access-Control-Allow-Methods ok");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.

test_db_request() ->
    Headers = [{"Origin", "http://example.com"}],
    Url = server() ++ "etap-test-db",
    case ibrowse:send_req(Url, Headers, get, []) of
    {ok, _, RespHeaders, _Body} ->
        etap:is(proplists:get_value("Access-Control-Allow-Origin", RespHeaders),
            "http://example.com",
            "db Access-Control-Allow-Origin ok");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.

test_db_preflight_request() ->
    Url = server() ++ "etap-test-db",
    Headers = [{"Origin", "http://example.com"},
               {"Access-Control-Request-Method", "GET"}],
    case ibrowse:send_req(Url, Headers, options, []) of
    {ok, _, RespHeaders, _} ->
        etap:is(proplists:get_value("Access-Control-Allow-Methods", RespHeaders),
                ?SUPPORTED_METHODS,
                "db Access-Control-Allow-Methods ok");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.


test_db_origin_request() ->
    Headers = [{"Origin", "http://example.com"}],
    Url = server() ++ "etap-test-db",
    case ibrowse:send_req(Url, Headers, get, []) of
    {ok, _, RespHeaders, _Body} ->
        etap:is(proplists:get_value("Access-Control-Allow-Origin", RespHeaders),
            "http://example.com",
            "db origin ok");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.

test_db1_origin_request() ->
    Headers = [{"Origin", "http://example.com"}],
    Url = server() ++ "etap-test-db1",
    case ibrowse:send_req(Url, Headers, get, [], [{host_header, "example.com"}]) of
    {ok, _, RespHeaders, _Body} ->
        etap:is(proplists:get_value("Access-Control-Allow-Origin", RespHeaders),
            "http://example.com",
            "db origin ok");
    _Else ->
        io:format("else ~p~n", [_Else]),
        etap:is(false, true, "ibrowse failed")
    end.

test_db_preflight_auth_request() ->
    Url = server() ++ "etap-test-db2",
    Headers = [{"Origin", "http://example.com"},
               {"Access-Control-Request-Method", "GET"}],
    case ibrowse:send_req(Url, Headers, options, []) of
    {ok, _Status, RespHeaders, _} ->
        etap:is(proplists:get_value("Access-Control-Allow-Methods", RespHeaders),
                ?SUPPORTED_METHODS,
                "db Access-Control-Allow-Methods ok");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.


test_db_origin_auth_request() ->
    Headers = [{"Origin", "http://example.com"}],
    Url = server() ++ "etap-test-db2",

    case ibrowse:send_req(Url, Headers, get, [],
        [{basic_auth, {"test", "test"}}]) of
    {ok, _, RespHeaders, _Body} ->
        etap:is(proplists:get_value("Access-Control-Allow-Origin", RespHeaders),
            "http://example.com",
            "db origin ok");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.

test_preflight_with_wildcard() ->
    Headers = [{"Origin", "http://example.com"},
               {"Access-Control-Request-Method", "GET"}],
    case ibrowse:send_req(server(), Headers, options, []) of
    {ok, _, RespHeaders, _}  ->
        % I would either expect the current origin or a wildcard to be returned
        etap:is(proplists:get_value("Access-Control-Allow-Origin", RespHeaders),
            "http://example.com",
            "db origin ok");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.

test_preflight_with_port1() ->
    Headers = [{"Origin", "http://example.com:5984"},
               {"Access-Control-Request-Method", "GET"}],
    case ibrowse:send_req(server(), Headers, options, []) of
    {ok, _, RespHeaders, _}  ->
        % I would either expect the current origin or a wildcard to be returned
        etap:is(proplists:get_value("Access-Control-Allow-Origin", RespHeaders),
            undefined,
            "check non defined host:port in origin ok");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.

test_preflight_with_port2() ->
    Headers = [{"Origin", "http://example.com:5984"},
               {"Access-Control-Request-Method", "GET"}],
    case ibrowse:send_req(server(), Headers, options, []) of
    {ok, _, RespHeaders, _}  ->
        % I would either expect the current origin or a wildcard to be returned
        etap:is(proplists:get_value("Access-Control-Allow-Origin", RespHeaders),
            "http://example.com:5984",
            "check host:port in origin ok");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.

test_preflight_with_scheme1() ->
    Headers = [{"Origin", "https://example.com:5984"},
               {"Access-Control-Request-Method", "GET"}],
    case ibrowse:send_req(server(), Headers, options, []) of
    {ok, _, RespHeaders, _}  ->
        % I would either expect the current origin or a wildcard to be returned
        etap:is(proplists:get_value("Access-Control-Allow-Origin", RespHeaders),
            undefined,
            "check non defined scheme in origin ok");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.

test_preflight_with_scheme2() ->
    Headers = [{"Origin", "https://example.com:5984"},
               {"Access-Control-Request-Method", "GET"}],
    case ibrowse:send_req(server(), Headers, options, []) of
    {ok, _, RespHeaders, _}  ->
        % I would either expect the current origin or a wildcard to be returned
        etap:is(proplists:get_value("Access-Control-Allow-Origin", RespHeaders),
            "https://example.com:5984",
            "check scheme in origin ok");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.
