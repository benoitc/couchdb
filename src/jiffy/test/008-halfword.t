#! /usr/bin/env escript
% This file is part of Jiffy released under the MIT license. 
% See the LICENSE file for more information.

main(_) ->
    test_util:run(2, fun() -> test() end).


test() ->
    etap:is(jiffy:decode(<<"1">>) =:= 1, true, "1 =:= 1"),
    etap:is(jiffy:decode(<<"1">>) == 1, true, "1 == 1"),
    ok.

