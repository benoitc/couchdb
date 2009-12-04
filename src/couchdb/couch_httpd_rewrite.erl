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

-module(couch_httpd_rewrite).
-export([handle_rewrite_req/2]).
-include("couch_db.hrl").


handle_rewrite_req(#httpd{
        path_parts=[DbName, <<"_rewrite">>, DesignName|PathParts]
    }=Req, Db) ->
        % we are in a db handler
        handle_rewrite_req1(DbName, DesignName, PathParts, Req, Db);   

handle_rewrite_req(#httpd{
        path_parts=[DbName, <<"_design">>, DesignName, _Rewrite|PathParts]
    }=Req, Db) ->
        % we are in a design handler
        handle_rewrite_req1(DbName, DesignName, PathParts, Req, Db).
        
handle_rewrite_req1(DbName, DesignName, PathParts, Req, Db) ->
    DesignId = <<"_design/", DesignName/binary>>,
    Prefix = <<"/", DbName/binary, "/", DesignId/binary>>,
    #doc{body={Props}} = couch_httpd_db:couch_doc_open(Db, DesignId, nil, []),
    Lang = proplists:get_value(<<"language">>, Props, <<"javascript">>),
    case proplists:get_value(<<"rewrite">>, Props) of
        undefined ->
            couch_httpd:send_error(Req, 404, <<"rewrite_error">>, <<"Invalid path.">>);
        FunSrc ->
            Req1 = Req#httpd{path_parts=PathParts},
            send_rewrite_response(Lang, FunSrc, ?b2l(Prefix), Req1, Db)
    end.
    
send_rewrite_response(Lang, FunSrc, Prefix, #httpd{mochi_req=MochiReq}=Req, Db) ->
    NewPath  = ?b2l(couch_query_servers:rewrite_path(Lang, FunSrc, Req, Db)),
    Path1 = case mochiweb_util:safe_relative_path(NewPath) of
        undefined -> Prefix ++ "/" ++ NewPath;
        P -> Prefix ++ "/" ++ P
    end,

    {"/" ++ Path2, _, _} = mochiweb_util:urlsplit_path(Path1),
    PathParts = [list_to_binary(couch_httpd:unquote(Part))
            || Part <- string:tokens(Path2, "/")],
    
    ?LOG_INFO("rewrite to ~p ~n", [NewPath]),

    couch_httpd_db:handle_request(Req#httpd{
                path_parts=PathParts,
                mochi_req=mochiweb_request:new(MochiReq:get(socket), 
                    MochiReq:get(method), Path1, MochiReq:get(version), 
                    MochiReq:get(headers))
    }).
    
    
    
    