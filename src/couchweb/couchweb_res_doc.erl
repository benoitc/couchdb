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

-module(couchweb_res_doc).
-export([
    init/1,
    allowed_methods/2,
    content_types_provided/2,
    content_types_accepted/2,
    resource_exists/2,
    is_conflict/2,
    delete_resource/2,
    delete_completed/2,
    to_json/2,
    to_text/2,
    from_json/2,
    couch_doc_open/4
]).

-include_lib("couchdb/couch_db.hrl").
-include_lib("webmachine/webmachine.hrl").

-record(ctx, {
    db,
    doc=nil,
    results=[],
    query_args,
    user_ctx
}).

-record(doc_query_args, {
    options = [],
    rev = nil,
    open_revs = [],
    update_type = interactive_edit,
    atts_since = nil
}).


init([]) ->
    %{{trace, "/tmp/traces"}, #ctx{}}. 
    {ok, #ctx{}}.
    
content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json},
      {"text/plain", to_text},
      {"text/html", to_text}],
     RD, Ctx}.
    
content_types_accepted(RD, Ctx) ->
    {[{"application/json", from_json},
    {"application/octet-stream", from_json}], RD, Ctx}.
    
allowed_methods(RD, Ctx) ->
    {['GET', 'HEAD', 'POST', 'PUT', 'DELETE'], RD, Ctx}.
    
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
    DocId =  wrq:path_info(docid, RD),
    UserCtx =  #user_ctx{}, 
    case couch_db:open(?l2b(DbName),  [{user_ctx, UserCtx}]) of
        {ok, Db} ->
            % parse query args
            QueryArgs = parse_doc_query(RD),
            #doc_query_args{
                rev = Rev,
                open_revs = Revs,
                options = Options,
                atts_since = AttsSince
            } = QueryArgs,
            
            case Revs of
            [] ->
                case couch_doc_open(Db, ?l2b(DocId), Rev, Options) of
                    {ok, Doc} ->
                        Options2 = if 
                            AttsSince /= nil ->
                                RevPos = find_ancestor_rev_pos(Doc#doc.revs, AttsSince),
                                [{atts_after_revpos, RevPos} | Options];
                                true -> Options
                        end,
                        QueryArgs1 = QueryArgs#doc_query_args{options=Options2},
                        {true, RD, Ctx#ctx{db=Db,doc=Doc,query_args=QueryArgs1}};
                    {error, not_found} ->
                        {false, RD, Ctx};
                    {error, Error} ->
                        {Code, Msg, Reason} = couchweb_utils:error_info(Error),
                        {{halt, Code},
                            wrq:append_to_response_body("~p, ~p.~n", [Msg, Reason], RD), 
                            Ctx}
                end;
            _ ->
                {ok, Results} = couch_db:open_doc_revs(Db, DocId, Revs, Options),
                {true, RD, Ctx#ctx{db=Db,results=Results,query_args=QueryArgs}}
                
            end;
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

delete_completed(RD, Ctx) ->
    {false, RD, Ctx}.

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

to_json(RD, Ctx=#ctx{doc=nil, results=Results}) ->
    #doc_query_args{
        options = Options
    } = Ctx#ctx.query_args,

    Body = fun(Write) ->
        Write("["),
        lists:foldl(
            fun(Result, AccSeparator) ->
                case Result of
                {ok, Doc} ->
                    JsonDoc = couch_doc:to_json_obj(Doc, Options),
                    Json = ?JSON_ENCODE({[{ok, JsonDoc}]}),
                    Write(AccSeparator ++ Json);
                {{not_found, missing}, RevId} ->
                    RevStr = couch_doc:rev_to_str(RevId),
                    Json = ?JSON_ENCODE({[{"missing", RevStr}]}),
                    Write(AccSeparator ++ Json)
                end,
                "," % AccSeparator now has a comma
            end, "", Results),
        Write("]" ++ "\n")
    end,
    
    {{writer, Body}, RD, Ctx};
    
to_json(RD, Ctx=#ctx{doc=Doc, results=_Results}) ->
    #doc_query_args{ options = Options} = Ctx#ctx.query_args,
    JsonObj = couch_doc:to_json_obj(Doc, Options),
    {?JSON_ENCODE(JsonObj), RD, Ctx}.

    
to_text(RD, C0) ->
    {BodyRst, RD1, C1} = to_json(RD, C0),
    Body1 = case BodyRst of
        {writer, Body} ->
            {writer, Body};
        Body ->
            json_pp:print(?b2l(?l2b(Body)))
    end,
    {Body1, RD1, C1}.
    
    
% private functions

find_ancestor_rev_pos({_, []}, _AttsSinceRevs) ->
    0;
find_ancestor_rev_pos(_DocRevs, []) ->
    0;
find_ancestor_rev_pos({RevPos, [RevId|Rest]}, AttsSinceRevs) ->
    case lists:member({RevPos, RevId}, AttsSinceRevs) of
    true ->
        RevPos;
    false ->
        find_ancestor_rev_pos({RevPos - 1, Rest}, AttsSinceRevs)
    end.
    
couch_doc_open(Db, DocId, Rev, Options) ->
    case Rev of
    nil -> % open most recent rev
        case couch_db:open_doc(Db, DocId, Options) of
        {ok, Doc} ->
            {ok, Doc};
        {not_found, missing} ->
            {error, not_found};
         Error ->
             {error, Error}
         end;
    _ -> % open a specific rev (deletions come back as stubs)
        case couch_db:open_doc_revs(Db, DocId, [Rev], Options) of
        {ok, [{ok, Doc}]} ->
            {ok, Doc};
        {ok, [{{not_found, missing}, Rev}]} ->
            {error, not_found};
        {ok, [Else]} ->
            {error, Else}
      end
  end.
  
parse_doc_query(RD) ->
    lists:foldl(fun({Key,Value}, Args) ->
        case {Key, Value} of
        {"attachments", "true"} ->
            Options = [attachments | Args#doc_query_args.options],
            Args#doc_query_args{options=Options};
        {"meta", "true"} ->
            Options = [revs_info, conflicts, deleted_conflicts | Args#doc_query_args.options],
            Args#doc_query_args{options=Options};
        {"revs", "true"} ->
            Options = [revs | Args#doc_query_args.options],
            Args#doc_query_args{options=Options};
        {"local_seq", "true"} ->
            Options = [local_seq | Args#doc_query_args.options],
            Args#doc_query_args{options=Options};
        {"revs_info", "true"} ->
            Options = [revs_info | Args#doc_query_args.options],
            Args#doc_query_args{options=Options};
        {"conflicts", "true"} ->
            Options = [conflicts | Args#doc_query_args.options],
            Args#doc_query_args{options=Options};
        {"deleted_conflicts", "true"} ->
            Options = [deleted_conflicts | Args#doc_query_args.options],
            Args#doc_query_args{options=Options};
        {"rev", Rev} ->
            Args#doc_query_args{rev=couch_doc:parse_rev(Rev)};
        {"open_revs", "all"} ->
            Args#doc_query_args{open_revs=all};
        {"open_revs", RevsJsonStr} ->
            JsonArray = ?JSON_DECODE(RevsJsonStr),
            Args#doc_query_args{open_revs=couch_doc:parse_revs(JsonArray)};
        {"atts_since", RevsJsonStr} ->
            JsonArray = ?JSON_DECODE(RevsJsonStr),
            Args#doc_query_args{atts_since = couch_doc:parse_revs(JsonArray)};
        {"new_edits", "false"} ->
            Args#doc_query_args{update_type=replicated_changes};
        {"new_edits", "true"} ->
            Args#doc_query_args{update_type=interactive_edit};
        _Else -> % unknown key value pair, ignore.
            Args
        end
    end, #doc_query_args{}, wrq:req_qs(RD)).
