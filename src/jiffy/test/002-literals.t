#! /usr/bin/env escript
% This file is part of Jiffy released under the MIT license. 
% See the LICENSE file for more information.

main(_) ->
    test_util:run(6, fun() -> test() end).

test() ->
    etap:is(jiffy:decode(<<"true">>), true, "DEC: true -> true"),
    etap:is(jiffy:encode(true), <<"true">>, "ENC: true -> true"),
    
    etap:is(jiffy:decode(<<"false">>), false, "DEC: false -> false"),
    etap:is(jiffy:encode(false), <<"false">>, "ENC: false -> false"),
    
    etap:is(jiffy:decode(<<"null">>), null, "DEC: null -> null"),
    etap:is(jiffy:encode(null), <<"null">>, "ENC: null -> null"),
    ok.


