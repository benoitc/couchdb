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

-module(couch_plugins).
-export([install/0, reinstall/0]).

install() ->
    %% build a list of plugins to load
    PluginsApps = find_plugins(),
    Apps = [App || {_Dir, App} <- PluginsApps],

    %% get lists of all applications needed. They are loaded in the same
    %% time.
    _AllPluginsApps = sets:to_list(load_plugins(Apps)),
    
    % save current plugins so we can uninstall them later.
    ets:new(?MODULE, [named_table, public]),
    ets:insert(?MODULE, {plugins, PluginsApps}),

    application:set_env(couch, plugins, PluginsApps),

    io:format("~w plugins activated:~n", [length(Apps)]),
    [io:format("* ~s~n", [App]) || App <- Apps],
    ok.

reinstall() ->
    case ets:lookup(?MODULE, plugins) of
        [] ->
            ok;
        [{plugins, OldPluginsApps}] ->
            lists:foreach(fun unload_plugin/1, OldPluginsApps)
    end,
    install().

find_plugins() ->
    PluginDir = couch_config:get("couchdb", "plugins_dir",
        filename:join(".", "plugins")),

    PluginsDirs = filelib:wildcard(PluginDir ++ "/*/ebin/*.app"),

    prepare_dir_plugin(PluginsDirs).

prepare_dir_plugin(PluginsDirs) ->
    prepare_dir_plugin(PluginsDirs, []).

prepare_dir_plugin([], Acc) ->
    Acc;
prepare_dir_plugin([PluginAppDescFn|Rest], Acc) ->
    PluginEBinDirN = filename:dirname(PluginAppDescFn),
    
    %% We want the second-last token
    NameTokens = string:tokens(PluginAppDescFn,"/."),
    PluginNameString = lists:nth(length(NameTokens) - 1, NameTokens),

    PluginName = list_to_atom(PluginNameString),

    case is_couchdb_app(PluginName) of
        true ->
            prepare_dir_plugin(Rest, Acc);
        false ->
            code:add_path(PluginEBinDirN),
            Acc1 = [{PluginEBinDirN, PluginName}|Acc],
            prepare_dir_plugin(Rest, Acc1)
    end.

is_couchdb_app(Name) ->
    lists:member(Name, [crypto, public_key, sasl, inets, oauth, ssl,
            ibrowse, mochiweb]).

load_plugins(Apps) ->
    load_plugins(sets:new(), Apps).

load_plugins(Current, []) ->
    Current;
load_plugins(Current, [App|Rest]) ->
    case sets:is_element(App, Current) of
        true ->
            load_plugins(Current, Rest);
        false ->
            case application:load(App) of
                ok ->
                    ok;
                {error, {already_loaded, _}} ->
                    ok;
                {error, Reason} ->
                    throw({failed_to_load_plugin, App, Reason})
            end,
            {ok, Required} = application:get_key(App, applications),
            Unique = [A || A <- Required, not(sets:is_element(A, Current))],
            load_plugins(sets:add_element(App, Current), Rest ++ Unique)
    end.

unload_plugin({Path, App}) ->
    application:stop(App),
    true = code:del_path(Path),
    ok.
