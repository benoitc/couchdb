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
% Some code borrowed to a Basho example under MIT License.
% @copyright 2008-2009 Basho Technologies, Inc.

-module(couchweb_res_static).
-export([init/1]).

-export([allowed_methods/2,
	 resource_exists/2,
	 last_modified/2,
	 content_types_provided/2,
	 moved_permanently/2,
	 previously_existed/2,
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
    
allowed_methods(RD, Ctx) ->
    {['HEAD', 'GET'], RD, Ctx}.

file_path(Ctx, Name) ->
   RelName = case Name of
        "" ->
            Name;
        _ ->
            case hd(Name) of
                "/" -> tl(Name);
                _ -> Name
            end
        end,
    filename:join([Ctx#ctx.root, RelName]).

file_exists(Ctx, Name) ->
    NamePath = file_path(Ctx, Name),
    case filelib:is_regular(NamePath) of 
	true ->
	    {true, NamePath};
	false ->
	    false
    end.
    
    
moved_permanently(RD, Ctx) ->
    case wrq:path(RD) of 
        "/_utils" ->
            {{true, "/_utils/"}, RD, Ctx};
        _ ->
            {false, RD, Ctx}
    end.
    

previously_existed(RD, Ctx) -> {true, RD, Ctx}.

resource_exists(RD, Ctx) ->
    case wrq:path(RD) of
        "/_utils" ->
            {false, RD, Ctx};
        _ ->
            Path = wrq:disp_path(RD),
            case file_exists(Ctx, Path) of 
        	{true, _} ->
        	    {true, RD, Ctx};
        	_ ->
                {false, RD, Ctx}
            end
    end.

maybe_fetch_object(Ctx, Path) ->
    % if returns {true, NewCtx} then NewCtx has response_body
    case Ctx#ctx.response_body of
	undefined ->
	    case file_exists(Ctx, Path) of 
		{true, FullPath} ->
		    {ok, Value} = file:read_file(FullPath),
		    {true, Ctx#ctx{response_body=Value}};
		false ->
		    {false, Ctx}
	    end;
	_Body ->
	    {true, Ctx}
    end.

content_types_provided(RD, #ctx{root=Root} = Ctx) ->
    Path = case wrq:disp_path(RD) of
        [] -> Root;
        P -> P
    end,
    
    CT = webmachine_util:guess_mime(Path),
    {[{CT, provide_content}], RD,
     Ctx#ctx{metadata=[{'content-type', CT}|Ctx#ctx.metadata]}}.


provide_content(RD, Ctx) ->
    case maybe_fetch_object(Ctx, wrq:disp_path(RD)) of 
	{true, NewCtx} ->
	    Body = NewCtx#ctx.response_body,
	    {Body, RD, Ctx};
	{false, NewCtx} ->
	    {error, RD, NewCtx}
    end.

last_modified(RD, Ctx) ->
    {true, FullPath} = file_exists(Ctx,
                                   wrq:disp_path(RD)),
    LMod = filelib:last_modified(FullPath),
    {LMod, RD, Ctx#ctx{metadata=[{'last-modified',
                    httpd_util:rfc1123_date(LMod)}|Ctx#ctx.metadata]}}.

hash_body(Body) -> mochihex:to_hex(binary_to_list(crypto:sha(Body))).

generate_etag(RD, Ctx) ->
    case maybe_fetch_object(Ctx, wrq:disp_path(RD)) of
        {true, BodyCtx} ->
            ETag = hash_body(BodyCtx#ctx.response_body),
            {ETag, RD,
             BodyCtx#ctx{metadata=[{etag,ETag}|
                                           BodyCtx#ctx.metadata]}};
        _ ->
            {undefined, RD, Ctx}
    end.

