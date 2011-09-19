#! /usr/bin/env escript
% This file is part of Jiffy released under the MIT license. 
% See the LICENSE file for more information.

main(_) ->
    test_util:run(18, fun() -> test() end).

test() ->
    jiffy_test_util:test_good(good()),
    jiffy_test_util:test_errors(errors()),
    ok.

good() ->
    [
        {<<"[]">>, []},
        {<<"[\t[\n]\r]">>, [[]], <<"[[]]">>},
        {<<"[\t123, \r true\n]">>, [123, true], <<"[123,true]">>},
        {<<"[1,\"foo\"]">>, [1, <<"foo">>]},
        {<<"[11993444355.0,1]">>, [11993444355.0,1], <<"[11993444355,1]">>},
        {
            <<"[\"\\u00A1\",\"\\u00FC\"]">>,
            [<<194, 161>>, <<195, 188>>],
            <<"[\"", 194, 161, "\",\"", 195, 188, "\"]">>
        }
    ].

errors() ->
    [
        <<"[">>,
        <<"]">>,
        <<"[,]">>,
        <<"[123">>,
        <<"[123,]">>,
        <<"[32 true]">>
    ].
