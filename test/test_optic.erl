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

from_test() ->
    ?assert(is_tuple(optic:from([id(), id()]))).

'%extend_test_'() ->
    New = fun (_Data, _Template) -> undefined end,
    [?_assert(is_tuple(optic:'%extend'(id(), #{}, New))),
     ?_assert(is_tuple(optic:'%extend'(id(), #{strict=>false}, New))),
     ?_assert(is_tuple(optic:'%extend'(id(), #{strict=>true}, New))),
     ?_assert(is_tuple(optic:'%extend'(id(), #{create=>undefined}, New))),
     ?_assertEqual(optic:'%extend'(id(), #{}, New),
                   optic:'%extend'(id(), #{strict=>false}, New)),
     ?_assertEqual(optic:'%extend'(id(), [], New),
                   optic:'%extend'(id(), #{strict=>false}, New)),
     ?_assertEqual(optic:'%extend'(id(), [strict], New),
                   optic:'%extend'(id(), #{strict=>true}, New)),
     ?_assertEqual(optic:'%extend'(id(), [create], New),
                   optic:'%extend'(id(), #{create=>true}, New))].

fold_test() ->
    ?assertEqual({ok, 2},
                 optic:fold([id()],
                            atom,
                            fun (atom, Acc) -> Acc + 1 end,
                            1)).

get_test() ->
    ?assertEqual({ok, [atom]},
                 optic:get([id()], atom)).

mapfold_test() ->
    ?assertEqual({ok, {modified, 2}},
                 optic:mapfold([id()],
                               atom,
                               fun (atom, Acc) -> {modified, Acc + 1} end,
                               1)).

map_test() ->
    ?assertEqual({ok, modified},
                 optic:map([id()],
                           atom,
                           fun (atom) -> modified end)).

put_test() ->
    ?assertEqual({ok, modified},
                 optic:put([id()], atom, modified)).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

id() ->
    MapFold = fun (Fun, Acc, Data) ->
        {ok, Fun(Data, Acc)}
    end,
    optic:new(MapFold).
