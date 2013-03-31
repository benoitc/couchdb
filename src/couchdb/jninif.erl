-module(jninif).

-export([cast/2, call/2, call/3, send_call/3]).
-on_load(on_load/0).

-define(default_timeout, 5000).

send(_Name, _Bin) ->
    {error, nif_not_loaded}.

cast(Name, Term) ->
    Bin = term_to_binary(Term),
    send(Name, Bin).

send_call(Name, Bin, Caller) ->
    send(Name, Bin),
    receive ReplyBin -> 
            Caller ! binary_to_term(ReplyBin)
    end.

call(Name, Term) ->
    call(Name, Term, ?default_timeout).

call(Name, Term, Timeout) ->
    Ref = make_ref(),
    Me = self(),
    Bin = term_to_binary({Ref, Term}),
    spawn(?MODULE, send_call, [Name, Bin, Me]),
    receive
        {Ref, Reply} ->
            {ok, Reply}
    after Timeout ->
            {error, timeout}
    end.

on_load() ->
    io:format("JNI NIF on_load called.~n"),
    {ok, LibPath} = init:get_argument(native_lib_path),
    Status = erlang:load_nif(hd(hd(LibPath)) ++ "/libcom_couchbase_android_ErlangThread", 0),
    case Status of
        ok -> ok;
        {error, {E, Str}} ->
            error_logger:error_msg("Error loading JNI NIF: ~p, ~s~n", [E,Str]),
            Status
    end.
