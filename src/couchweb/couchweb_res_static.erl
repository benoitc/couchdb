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
%% @copyright 2008-2009 Basho Technologies, Inc.

-module(couchweb_res_static).
-export([init/1]).

-export([allowed_methods/2,
	 resource_exists/2,
	 last_modified/2,
	 content_types_provided/2,
	 provide_content/2,
	 generate_etag/2]).


-record(ctx, {
    root,
    response_body=undefined,
    metadata=[]}).

-include_lib("kernel/include/file.hrl").
-include_lib("webmachine/webmachine.hrl").

init(ConfigProps) ->
    {root, Root} = proplists:lookup(root, ConfigProps),
    {ok, #ctx{root=Root}}.
    
allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET'], ReqData, Context}.

file_path(Context, Name) ->
   RelName = case Name of
        "" ->
            Name;
        _ ->
            case hd(Name) of
                "/" -> tl(Name);
                _ -> Name
            end
        end,
    filename:join([Context#ctx.root, RelName]).

file_exists(Context, Name) ->
    NamePath = file_path(Context, Name),
    case filelib:is_regular(NamePath) of 
	true ->
	    {true, NamePath};
	false ->
	    false
    end.

resource_exists(ReqData, Context) ->
    Path = wrq:disp_path(ReqData),
    case file_exists(Context, Path) of 
	{true, _} ->
	    {true, ReqData, Context};
	_ ->
        {false, ReqData, Context}
    end.

maybe_fetch_object(Context, Path) ->
    % if returns {true, NewContext} then NewContext has response_body
    case Context#ctx.response_body of
	undefined ->
	    case file_exists(Context, Path) of 
		{true, FullPath} ->
		    {ok, Value} = file:read_file(FullPath),
		    {true, Context#ctx{response_body=Value}};
		false ->
		    {false, Context}
	    end;
	_Body ->
	    {true, Context}
    end.

content_types_provided(ReqData, #ctx{root=Root} = Ctx) ->
    io:format("path ~p ~n",[ wrq:disp_path(ReqData)]),
    
    
    Path = case wrq:disp_path(ReqData) of
        [] -> Root;
        P -> P
    end,
    
    CT = webmachine_util:guess_mime(Path),
    {[{CT, provide_content}], ReqData,
     Ctx#ctx{metadata=[{'content-type', CT}|Ctx#ctx.metadata]}}.


provide_content(ReqData, Ctx) ->
    case maybe_fetch_object(Ctx, wrq:disp_path(ReqData)) of 
	{true, NewContext} ->
	    Body = NewContext#ctx.response_body,
	    {Body, ReqData, Ctx};
	{false, NewContext} ->
	    {error, ReqData, NewContext}
    end.

last_modified(ReqData, Context) ->
    {true, FullPath} = file_exists(Context,
                                   wrq:disp_path(ReqData)),
    LMod = filelib:last_modified(FullPath),
    {LMod, ReqData, Context#ctx{metadata=[{'last-modified',
                    httpd_util:rfc1123_date(LMod)}|Context#ctx.metadata]}}.

hash_body(Body) -> mochihex:to_hex(binary_to_list(crypto:sha(Body))).

generate_etag(ReqData, Context) ->
    case maybe_fetch_object(Context, wrq:disp_path(ReqData)) of
        {true, BodyContext} ->
            ETag = hash_body(BodyContext#ctx.response_body),
            {ETag, ReqData,
             BodyContext#ctx{metadata=[{etag,ETag}|
                                           BodyContext#ctx.metadata]}};
        _ ->
            {undefined, ReqData, Context}
    end.

