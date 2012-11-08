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

%% @doc module to handle Cross-Origin Resource Sharing
%%
%% This module handles CROSS requests and preflight request for a
%% couchdb Node. The config is done in the ini file.


-module(couch_httpd_cors).

-include("couch_db.hrl").

-export([is_preflight_request/1, cors_headers/1]).

-define(SUPPORTED_HEADERS, "Accept, Accept-Language, Content-Type," ++
        "Expires, Last-Modified, Pragma, Origin, Content-Length," ++
        "If-Match, Destination, X-Requested-With, " ++
        "X-Http-Method-Override, Content-Range").

-define(SUPPORTED_METHODS, "GET, HEAD, POST, PUT, DELETE," ++
        "TRACE, CONNECT, COPY, OPTIONS").

% TODO: - pick a sane default
%       - make configurable
-define(CORS_DEFAULT_MAX_AGE, 12345).

is_preflight_request(#httpd{method=Method}=Req) when Method /= 'OPTIONS' ->
    Req;
is_preflight_request(Req) ->
    EnableCors = enable_cors(),
    is_preflight_request(Req, EnableCors).

is_preflight_request(Req, false) ->
    Req;
is_preflight_request(#httpd{mochi_req=MochiReq}=Req, true) ->
    case preflight_request(MochiReq) of
    {ok, PreflightHeaders} ->
        send_preflight_response(Req, PreflightHeaders);
    _ ->
        Req
    end.

cors_headers(MochiReq) ->
    EnableCors = enable_cors(),
    cors_headers(MochiReq, EnableCors).

cors_headers(#httpd{mochi_req=MochiReq}, true) ->
    Host = couch_httpd_vhost:host(MochiReq),
    AcceptedOrigins = split_list(cors_config(Host, "origins", [])),
    case MochiReq:get_header_value("Origin") of
    undefined ->
        [];
    Origin ->
        handle_cors_headers(couch_util:to_list(Origin),
                            Host, AcceptedOrigins)
    end;
cors_headers(_MochiReq, false) ->
    [].

handle_cors_headers(_Origin, _Host, []) ->
    [];
handle_cors_headers(Origin, Host, AcceptedOrigins) ->
    AcceptAll = lists:member("*", AcceptedOrigins),
    case {AcceptAll, lists:member(Origin, AcceptedOrigins)} of
    {true, _} ->
        make_cors_header(Origin, Host);
    {false, true}  ->
        make_cors_header(Origin, Host);
    _ ->
        []
    end.

make_cors_header(Origin, Host) ->
    case credentials(Origin, Host) of
    true ->
        [{"Access-Control-Allow-Origin", Origin},
         {"Access-Control-Allow-Credentials", "true"}];
    false ->
        [{"Access-Control-Allow-Origin", Origin}]
    end.

preflight_request(MochiReq) ->
    Host = couch_httpd_vhost:host(MochiReq),
    case MochiReq:get_header_value("Origin") of
    undefined ->
        MochiReq;

    Origin ->
        AcceptedOrigins = split_list(cors_config(Host, "origins", [])),
        AcceptAll = lists:member("*", AcceptedOrigins),

        case {AcceptAll, AcceptedOrigins} of
        {true, _} ->
            handle_preflight_request(couch_util:to_list(Origin),
                                     Host, MochiReq);
        {false, _} ->
            case lists:member(Origin, AcceptedOrigins) of
            true ->
                handle_preflight_request(couch_util:to_list(Origin),
                                         Host, MochiReq);
            false ->
                false
            end
        end
    end.

handle_preflight_request(Origin, Host, MochiReq) ->
    %% get supported methods
    SupportedMethods = split_list(cors_config(Host, "methods",
                                              ?SUPPORTED_METHODS)),

    % get supported headers
    AllSupportedHeaders = split_list(cors_config(Host, "headers",
                                                 ?SUPPORTED_HEADERS)),

    SupportedHeaders = [string:to_lower(H) || H <- AllSupportedHeaders],

    % get max age
    MaxAge = cors_config(Host, "max_age", ?CORS_DEFAULT_MAX_AGE),

    PreflightHeaders0 = case credentials(Origin, Host) of
    true ->
        [{"Access-Control-Allow-Origin", Origin},
         {"Access-Control-Allow-Credentials", "true"},
         {"Access-Control-Max-Age", MaxAge},
         {"Access-Control-Allow-Methods", string:join(SupportedMethods,
                                                      ", ")}];
    false ->
        [{"Access-Control-Allow-Origin", Origin},
         {"Access-Control-Max-Age", MaxAge},
         {"Access-Control-Allow-Methods", string:join(SupportedMethods,
                                                      ", ")}]
    end,

    case MochiReq:get_header_value("Access-Control-Request-Method") of
    undefined ->
        {ok, PreflightHeaders0};
    Method ->
        case lists:member(Method, SupportedMethods) of
        true ->
            % method ok , check headers
            AccessHeaders = MochiReq:get_header_value(
                    "Access-Control-Request-Headers"),
            {FinalReqHeaders, ReqHeaders} = case AccessHeaders of
                undefined -> {"", []};
                Headers ->
                    % transform header list in something we
                    % could check. make sure everything is a
                    % list
                    RH = [string:to_lower(H)
                          || H <- split_headers(Headers)],
                    {Headers, RH}
            end,
            % check if headers are supported
            case ReqHeaders -- SupportedHeaders of
            [] ->
                PreflightHeaders = PreflightHeaders0 ++
                                   [{"Access-Control-Allow-Headers",
                                     FinalReqHeaders}],
                {ok, PreflightHeaders};
            _ ->
                false
            end;
        false ->
            false
        end
    end.


send_preflight_response(#httpd{mochi_req=MochiReq}=Req, Headers) ->
    couch_httpd:log_request(Req, 204),
    couch_stats_collector:increment({httpd_status_codes, 204}),
    Headers1 = couch_httpd:http_1_0_keep_alive(MochiReq, Headers),
    Headers2 = Headers1 ++ couch_httpd:server_header() ++
               couch_httpd_auth:cookie_auth_header(Req, Headers1),
    {ok, MochiReq:respond({204, Headers2, <<>>})}.


credentials("*", _Host) ->
    false;
credentials(_Origin, Host) ->
    Default = get_bool_config("cors", "credentials", false),
    get_bool_config(cors_section(Host), "credentials", Default).


cors_config(Host, Key, Default) ->
    couch_config:get(cors_section(Host), Key,
                     couch_config:get("cors", Key, Default)).

cors_section(Host0) ->
    {Host, _Port} = split_host_port(Host0),
    "cors:" ++ Host.

enable_cors() ->
    get_bool_config("httpd", "enable_cors", false).

get_bool_config(Section, Key, Default) ->
    case couch_config:get(Section, Key) of
    undefined ->
        Default;
    "true" ->
        true;
    "false" ->
        false
    end.

split_list(S) ->
    re:split(S, "\\s*,\\s*", [trim, {return, list}]).

split_headers(H) ->
    re:split(H, ",\\s*", [{return,list}, trim]).

split_host_port(HostAsString) ->
    case string:rchr(HostAsString, $:) of
    0 ->
        {HostAsString, '*'};
    N ->
        HostPart = string:substr(HostAsString, 1, N-1),
        case (catch erlang:list_to_integer(string:substr(HostAsString,
                        N+1, length(HostAsString)))) of
        {'EXIT', _} ->
            {HostAsString, '*'};
        Port ->
            {HostPart, Port}
        end
    end.
