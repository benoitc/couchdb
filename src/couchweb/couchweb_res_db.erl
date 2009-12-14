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

-module(couchweb_res_db).
-export([
    init/1,
    allowed_methods/2,
    content_types_provided/2,
    content_types_accepted/2,
    resource_exists/2,
    is_conflict/2,
    is_authorized/2,
    delete_resource/2,
    to_json/2,
    to_text/2,
    from_json/2
]).

-include_lib("couchdb/couch_db.hrl").
-include_lib("webmachine/webmachine.hrl").

-record(ctx, {
    db,
    user_ctx
}).

init([]) ->
    {{trace, "/tmp/traces"}, #ctx{}}. 
    %%{ok, #ctx{}}.
    
content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json},
      {"text/plain", to_text},
      {"text/html", to_text}],
     RD, Ctx}.
    
content_types_accepted(RD, Ctx) ->
    {[{"application/json", from_json},
    {"application/octet-stream", from_json}], RD, Ctx}.
    
allowed_methods(RD, Ctx) ->
    {['GET', 'HEAD', 'PUT', 'DELETE'], RD, Ctx}.
    
is_authorized(RD, Ctx) ->
    {true, RD, Ctx}.
    
is_conflict(RD, Ctx) ->
    DbName = wrq:path_info(dbname, RD),
    UserCtx =  #user_ctx{}, 
    case couch_db:open(?l2b(DbName),  [{user_ctx, UserCtx}]) of
        {ok, Db} ->
            couch_db:close(Db),
            {true, RD, Ctx};
        _ ->
            {false, RD, Ctx}
    end.

resource_exists(RD, Ctx) ->
    DbName = wrq:path_info(dbname, RD),
    UserCtx =  #user_ctx{}, 
    case couch_db:open(?l2b(DbName),  [{user_ctx, UserCtx}]) of
        {ok, Db} ->
            {true, RD, Ctx#ctx{db=Db}};
        _Error ->
            {false, RD, Ctx}
    end.

delete_resource(RD, Ctx) ->
    DbName = wrq:path_info(dbname, RD),
    UserCtx =  #user_ctx{},
    case couch_server:delete(?l2b(DbName), [{user_ctx, UserCtx}]) of
    ok ->
        RD1 = wrq:append_to_response_body(?JSON_ENCODE({[{ok, true}]}), RD),
        {true, RD1, Ctx};
    Error ->
        {Code, Msg, Reason} = couchweb_utils:error_info(Error),
        {{halt, Code},
            wrq:append_to_response_body("~p, ~p.~n", [Msg, Reason], RD), 
            Ctx}
    end.
    

from_json(RD, Ctx) ->
    DbName = wrq:path_info(dbname, RD),
    UserCtx =  #user_ctx{},
    case couch_server:create(?l2b(DbName), [{user_ctx, UserCtx}]) of
        {ok, Db} ->
            couch_db:close(Db),
            DocUrl = "/" ++ couch_util:url_encode(DbName),
            RD1 = wrq:append_to_response_body(?JSON_ENCODE({[{ok, true}]}), RD),
            {true, wrq:set_resp_header("Location", DocUrl, RD1), Ctx};
        Error ->
            {Code, Msg, Reason} = couchweb_utils:error_info(Error),
            {{halt, Code},
                wrq:append_to_response_body("~p, ~p.~n", [Msg, Reason], RD), 
                Ctx}
    end.

to_json(RD, Ctx=#ctx{db=Db}) ->
    {ok, DbInfo} = couch_db:get_db_info(Db),
    {?JSON_ENCODE({DbInfo}) ++ <<"\n">>, RD, Ctx}.
    
to_text(RD, C0) ->
    {Json, RD1, C1} = to_json(RD, C0),
    {json_pp:print(binary_to_list(list_to_binary(Json))), RD1, C1}.
