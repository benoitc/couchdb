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
        mochi_req=MochiReq}=Req, _Db, DDoc) ->
    
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
            RawPath = case try_bind_path(DispatchList, Method, PathParts, 
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
                    case mochiweb_util:safe_relative_path(Path) of
                        undefined -> Prefix ++ "/" ++ ?l2b(Path);
                        P1 -> ?b2l(Prefix) ++ "/" ++ P1
                    end
                   
                end,
            RawPath1 = ?b2l(iolist_to_binary(RawPath)),
            {"/" ++ NewPath2, _, _} = mochiweb_util:urlsplit_path(RawPath1),
            NewPathParts1 = [list_to_binary(couch_httpd:unquote(Part))
                            || Part <- string:tokens(NewPath2, "/")],
                            
            ?LOG_INFO("rewrite to ~p ~n", [RawPath1]),

            MochiReq1 = mochiweb_request:new(MochiReq:get(socket),
                                             MochiReq:get(method),
                                             RawPath1,
                                             MochiReq:get(version),
                                             MochiReq:get(headers)),
            MochiReq1:cleanup(),
                        
            couch_httpd_db:handle_request(Req#httpd{
                        path_parts=NewPathParts1,
                        mochi_req=MochiReq1})
        end.
            

try_bind_path([], _Method, _PathParts, _QueryList) ->
    no_dispatch_path;        
try_bind_path([Dispatch|Rest], Method, PathParts, QueryList) ->
    [{PathParts1, Method1}, RedirectPath, QueryArgs] = Dispatch,
    case bind_method(Method1, Method) of
        true ->
            case bind_path(PathParts1, lists:reverse(PathParts), []) of
                {ok, Remaining, Bindings} ->
                    Bindings1 = Bindings ++ QueryList,
                    
                    % we parse query args from the rule and fill 
                    % it eventually with bindings var
                    Bindings2 = Bindings1 ++ make_query_list(QueryArgs, 
                                                    Bindings1, []),
                                                    
                    % keep only unique value
                    FinalBindings = lists:foldl(fun ({K, V}, Acc) ->
                        KV = case proplists:is_defined(K, Acc) of
                            true -> [];
                            false ->
                                [{K, V}]
                            end,
                            Acc ++ KV
                    end, [], Bindings2),
                    
                    NewPathParts = make_new_path(RedirectPath, FinalBindings, 
                                    Remaining, []),
                                    
                    {NewPathParts, FinalBindings};         
                fail ->
                    try_bind_path(Rest, Method, PathParts, QueryList)
            end;    
        false ->
            try_bind_path(Rest, Method, PathParts, QueryList)
    end.
    
make_query_list([], _Bindings, Acc) ->
    Acc;
make_query_list([{Key, {Value}}|Rest], Bindings, Acc) ->
    Value1 = make_query_list(Value, Bindings, []),
    make_query_list(Rest, Bindings, [{to_atom(Key), Value1}|Acc]);
make_query_list([{Key, Value}|Rest], Bindings, Acc) when is_binary(Value) ->
    Value1 = replace_var(Value, Bindings),
    make_query_list(Rest, Bindings, [{to_atom(Key), Value1}|Acc]);
make_query_list([{Key, Value}|Rest], Bindings, Acc) when is_list(Value) ->    
    Value1 = replace_vars(Value, Bindings, []),
    make_query_list(Rest, Bindings, [{to_atom(Key), Value1}|Acc]);
make_query_list([{Key, Value}|Rest], Bindings, Acc) ->
    make_query_list(Rest, Bindings, [{to_atom(Key), Value}|Acc]).
    
replace_vars([], _Bindings, Acc) ->
    lists:reverse(Acc);
replace_vars([V|R], Bindings, Acc) ->
    V1 = replace_var(V, Bindings),
    replace_vars(R, Bindings, [V1|Acc]).
    
replace_var(Value, Bindings) ->
    case Value of
        <<":", Var/binary>> ->
            Var1 = list_to_atom(binary_to_list(Var)),
            io:format("var ~p ~p ~p ~n", [Var1, Bindings, proplists:get_value(Var1, Bindings, Value)]),
            proplists:get_value(Var1, Bindings, Value);
        _ -> Value
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
    Method = case proplists:get_value(<<"method">>, Rule) of
        undefined -> '*';
        M -> list_to_atom(?b2l(M))
    end,
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
    path_to_list(re:split(Path, SlashRE), []).
    

path_to_list([], Acc) ->
    Acc;
path_to_list([<<>>|R], Acc) ->
    path_to_list(R, Acc);
path_to_list([<<"*">>|R], Acc) ->
    path_to_list(R, [?MATCH_ALL|Acc]);
path_to_list([P|R], Acc) ->
    P1 = case P of
        <<":", Var/binary>> ->
            list_to_atom(binary_to_list(Var));
        _ -> P
    end,
    path_to_list(R, [P1|Acc]).

encode_query(Props) ->
    RevPairs = lists:foldl(fun ({K, V}, Acc) ->
                                V1 = case K of
                                    <<"endkey">> ->
                                        iolist_to_binary(?JSON_ENCODE(V));
                                    <<"key">> ->
                                        iolist_to_binary(?JSON_ENCODE(V));
                                    <<"startkey">> ->
                                        iolist_to_binary(?JSON_ENCODE(V));
                                    _ -> V
                                end,
                                        
        
                                [{K, V1} | Acc]
                           end, [], Props),
    lists:flatten(mochiweb_util:urlencode(RevPairs)).
    
to_atom(V) when is_binary(V) ->
    to_atom(?b2l(V));
to_atom(V) ->
    list_to_atom(V).
    
