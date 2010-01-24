% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.
%
% bind_path is based on bind method from Webmachine

-module(couch_httpd_rewrite).
-export([handle_rewrite_req/3]).
-include("couch_db.hrl").

-define(SEPARATOR, $\/).
-define(MATCH_ALL, '*').

handle_rewrite_req(#httpd{
        path_parts=[DbName, <<"_design">>, DesignName, _Rewrite|PathParts],
        method=Method,
        mochi_req=MochiReq}=Req, Db, DDoc) ->
    
    % we are in a design handler
    DesignId = <<"_design/", DesignName/binary>>,
    Prefix = <<"/", DbName/binary, "/", DesignId/binary>>,
    QueryList = couch_httpd:qs(Req),
    
    
        
    #doc{body={Props}} = DDoc,
    case proplists:get_value(<<"rewrites">>, Props) of
        undefined ->
            couch_httpd:send_error(Req, 404, <<"rewrite_error">>, 
                            <<"Invalid path.">>);
        Rules ->
            DispatchList =  [make_rule(Rule) || {Rule} <- Rules],
            NewPath = case try_bind_path(DispatchList, Method, PathParts, 
                                    QueryList) of
                no_dispatch_path ->
                    throw(not_found);
                {NewPathParts, Bindings} -> 
                    Parts = [mochiweb_util:quote_plus(X) || X <- NewPathParts],
                    Path = lists:append(
                        string:join(Parts, [?SEPARATOR]),
                        case Bindings of
                            [] -> [];
                            _ -> [$?, encode_query(Bindings)]
                        end),
                    io:format("ici"),  
                    case mochiweb_util:safe_relative_path(Path) of
                        undefined -> Prefix ++ "/" ++ ?l2b(Path);
                        P1 -> ?b2l(Prefix) ++ "/" ++ P1
                    end
                   
                end,
            {"/" ++ NewPath2, _, _} = mochiweb_util:urlsplit_path(NewPath),
            NewPathParts1 = [list_to_binary(couch_httpd:unquote(Part))
                            || Part <- string:tokens(NewPath2, "/")],
            ?LOG_INFO("rewrite to ~p ~n", [iolist_to_binary(NewPath)]),

            couch_httpd_db:handle_request(Req#httpd{
                        path_parts=NewPathParts1,
                        mochi_req=mochiweb_request:new(MochiReq:get(socket),
                        MochiReq:get(method), iolist_to_binary(NewPath), MochiReq:get(version),
                        MochiReq:get(headers))})
        end.
            

try_bind_path([], _Method, _PathParts, _QueryList) ->
    no_dispatch_path;        
try_bind_path([Dispatch|Rest], Method, PathParts, QueryList) ->
    [{PathParts1, Method1}, RedirectPath, QueryArgs] = Dispatch,
    case bind_method(Method1, Method) of
        true ->
            case bind_path(PathParts1, lists:reverse(PathParts), []) of
                {ok, Remaining, Bindings} ->
                    % Update QueryList, by default we remove items set 
                    % in QueryArgs
                    QueryList1 = lists:foldl(fun({K, V}, Acc) ->
                            RevPair = case proplist:is_defined(K, QueryArgs) of
                                true -> [];
                                false -> [{K, ?JSON_DECODE(V)}]
                            end,
                            RevPair ++ Acc
                        end, [], QueryList),
                    QueryParams = QueryList1 ++ QueryArgs,
                    Bindings1 = Bindings ++ QueryParams,
                    
                    NewPathParts = make_new_path(RedirectPath, Bindings1, 
                                    Remaining, []),
                    {NewPathParts, Bindings1};         
                fail ->
                    try_bind_path(Rest, Method, PathParts, QueryList)
            end;    
        false ->
            try_bind_path(Rest, Method, PathParts, QueryList)
    end.


make_new_path([], _Bindings, _Remaining, Acc) ->
    Acc;
make_new_path([?MATCH_ALL], _Bindings, Remaining, Acc) ->
    Acc1 = Acc ++ Remaining,
    Acc1;
make_new_path([?MATCH_ALL|_Rest], _Bindings, Remaining, Acc) ->
    Acc1 = Acc ++ Remaining,
    Acc1;
make_new_path([P|Rest], Bindings, Remaining, Acc) when is_atom(P) ->
    P2 = case proplists:get_value(P, Bindings) of 
        undefined -> << "undefined">>;
        P1 -> P1
    end,
    make_new_path(Rest, Bindings, Remaining, [P2|Acc]);
make_new_path([P|Rest], Bindings, Remaining, Acc) ->
    make_new_path(Rest, Bindings, Remaining, [P|Acc]).
            
    
bind_method(?MATCH_ALL, _Method) ->
    true;
bind_method(Method, Method) ->
    true;
bind_method(_, _) ->
    false. 

bind_path([], [], Bindings) ->
    {ok, [], Bindings};
bind_path([?MATCH_ALL], Rest, Bindings) when is_list(Rest) ->
    {ok, Rest, Bindings};
bind_path(_, [], _) ->
    fail;
bind_path([Token|RestToken],[Match|RestMatch],Bindings) when is_atom(Token) ->
    bind_path(RestToken, RestMatch, [{Token, Match}|Bindings]);
bind_path([Token|RestToken], [Token|RestMatch], Bindings) ->
    bind_path(RestToken, RestMatch, Bindings);
bind_path(_, _, _) ->
    fail.

    
make_rule(Rule) ->
    Method = proplists:get_value(<<"method">>, Rule, '*'),
    QueryArgs = case proplists:get_value(<<"query">>, Rule) of
        undefined -> [];
        {Args} -> Args
        end,
    FromParts  = case proplists:get_value(<<"from">>, Rule) of
        undefined -> ['*'];
        From ->
            parse_path(From)
        end,
    ToParts  = case proplists:get_value(<<"to">>, Rule) of
        undefined ->  
            throw({error, invalid_rewrite_target});
        To ->
            parse_path(To)
        end,
    [{FromParts, Method}, ToParts, QueryArgs].
    
parse_path(Path) ->
    {ok, SlashRE} = re:compile(<<"\\/">>),
    path_to_erlang(re:split(Path, SlashRE), []).
    

path_to_erlang([], Acc) ->
    Acc;
path_to_erlang([<<>>|R], Acc) ->
    path_to_erlang(R, Acc);
path_to_erlang([<<"*">>|R], Acc) ->
    path_to_erlang(R, [?MATCH_ALL|Acc]);
path_to_erlang([P|R], Acc) ->
     ?LOG_ERROR("got: ~p", [P]),
    P1 = case P of
        <<":", Var/binary>> ->
            list_to_atom(binary_to_list(Var));
        _ -> P
    end,
    path_to_erlang(R, [P1|Acc]).

encode_query(Props) ->
    RevPairs = lists:foldl(fun ({K, V}, Acc) ->
                                   [{K, iolist_to_binary(?JSON_ENCODE(V))} | Acc]
                           end, [], Props),
    lists:flatten(mochiweb_util:urlencode(RevPairs)).
