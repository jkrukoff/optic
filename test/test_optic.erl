%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for src/optic.erl
%%% @end
%%%-------------------------------------------------------------------
-module(test_optic).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

new_test_() ->
    Fold = fun (Fun, Acc, Data) ->
        {ok, Fun(Data, Acc)}
    end,
    [?_assert(is_tuple(optic:new(Fold))),
     ?_assert(is_tuple(optic:new(Fold, Fold)))].

%%%===================================================================
%%% Internal Functions
%%%===================================================================

id() ->
    Fold = fun (Fun, Acc, Data) ->
        {ok, Fun(Data, Acc)}
    end,
    optic:new(Fold, Fold).
