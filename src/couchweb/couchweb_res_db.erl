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
    is_authorized/2,
    to_json/2,
    to_text/2
]).

-include_lib("couchdb/couch_db.hrl").
-include_lib("webmachine/webmachine.hrl").

-record(ctx, {
    db,
    user_ctx
}).

init([]) ->
    {ok, #ctx{}}.
    
content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json},
      {"text/plain", to_text},
      {"text/html", to_text}],
     RD, Ctx}.
     
content_types_accepted(RD, Ctx) ->
    {[{"applicatiob/json", handle_incoming}], RD, Ctx}.
    
allowed_methods(RD, Ctx) ->
    {['GET', 'HEAD', 'DELETE', 'POST', 'PUT'], 
    RD, Ctx}.
    
is_authorized(RD, Ctx) ->
    io:format("auth"),
    {true, RD, Ctx}.
     
resource_exists(RD, Ctx) ->
    io:format("exist"),
    DbName = wrq:path_info(dbname, RD),
    UserCtx =  #user_ctx{}, 
    case couch_db:open(?l2b(DbName),  [{user_ctx, UserCtx}]) of
        {ok, Db} ->
            {true, RD, Ctx#ctx{db=Db}};
        _Error ->
            {false, RD, Ctx}
    end.
       
to_json(RD, Ctx=#ctx{db=Db}) ->
    {ok, DbInfo} = couch_db:get_db_info(Db),
    {?JSON_ENCODE({DbInfo}) ++ <<"\n">>, RD, Ctx}.
    

to_text(RD, C0) ->
    {Json, RD1, C1} = to_json(RD, C0),
    {json_pp:print(binary_to_list(list_to_binary(Json))), RD1, C1}.
    
handle_incoming(RD, Ctx) ->
    {RD, Ctx}.
    
