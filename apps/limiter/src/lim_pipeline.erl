%%%
%%% Pipeline
%%%

-module(lim_pipeline).

-export([do/1]).
-export([do/2]).
-export([unwrap/1]).
-export([unwrap/2]).
-export([expect/2]).
-export([valid/2]).

%%

-type thrown(_E) ::
    no_return().

-type result(T, E) ::
    {ok, T} | {error, E}.

-spec do(fun(() -> ok | T | thrown(E))) -> ok | result(T, E).
do(Fun) ->
    try Fun() of
        ok ->
            ok;
        R ->
            {ok, R}
    catch
        throw:Thrown -> {error, Thrown}
    end.

-spec do(Tag, fun(() -> ok | T | thrown(E))) -> ok | result(T, {Tag, E}).
do(Tag, Fun) ->
    do(fun() -> unwrap(Tag, do(Fun)) end).

-spec unwrap
    (ok) -> ok;
    ({ok, V}) -> V;
    ({error, E}) -> thrown(E).
unwrap(ok) ->
    ok;
unwrap({ok, V}) ->
    V;
unwrap({error, E}) ->
    throw(E).

-spec expect
    (_E, ok) -> ok;
    (_E, {ok, V}) -> V;
    (E, {error, _}) -> thrown(E).
expect(_, ok) ->
    ok;
expect(_, {ok, V}) ->
    V;
expect(E, {error, _}) ->
    throw(E).

-spec unwrap
    (_Tag, ok) -> ok;
    (_Tag, {ok, V}) -> V;
    (Tag, {error, E}) -> thrown({Tag, E}).
unwrap(_, ok) ->
    ok;
unwrap(_, {ok, V}) ->
    V;
unwrap(Tag, {error, E}) ->
    throw({Tag, E}).

-spec valid(T, T) -> ok | {error, T}.
valid(V, V) ->
    ok;
valid(_, V) ->
    {error, V}.
