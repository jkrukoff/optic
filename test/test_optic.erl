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
    [?_assert(optic:is_optic(optic:new(Fold))),
     ?_assert(optic:is_optic(optic:new(Fold, Fold)))].

wrap_test_() ->
    Wrapped2 = optic:wrap(
                 id(),
                 fun (MapFold) ->
                         MapFold
                 end),
    Wrapped3 = optic:wrap(
                 id(),
                 fun (MapFold) ->
                         MapFold
                 end,
                 fun (Fold) ->
                         Fold
                 end),
    [?_assert(optic:is_optic(Wrapped2)),
     ?_assert(optic:is_optic(Wrapped3)),
     ?_assertEqual({ok, [atom]},
                   optic:get(Wrapped2, atom)),
     ?_assertEqual({ok, modified},
                   optic:put(Wrapped2, modified, atom)),
     ?_assertEqual({ok, [atom]},
                   optic:get(Wrapped3, atom)),
     ?_assertEqual({ok, modified},
                   optic:put(Wrapped3, modified, atom))].

chain_test_() ->
    [?_assert(optic:is_optic(optic:chain([id(), id(), id()]))),
     ?_assertEqual({ok, [atom]},
                   optic:get(optic:chain([id()]),
                             atom)),
     ?_assertEqual({ok, [atom]},
                   optic:get(optic:chain([id(), id()]),
                             atom)),
     ?_assertEqual({ok, [atom]},
                   optic:get(optic:chain([id(), id(), id()]),
                             atom)),
     ?_assertEqual({ok, modified},
                   optic:put(optic:chain([id()]),
                             modified,
                             atom)),
     ?_assertEqual({ok, modified},
                   optic:put(optic:chain([id(), id()]),
                             modified,
                             atom)),
     ?_assertEqual({ok, modified},
                   optic:put(optic:chain([id(), id(), id()]),
                             modified,
                             atom))].

merge_test_() ->
    [?_assert(optic:is_optic(optic:merge([id(), id(), id()]))),
     ?_assertEqual({ok, [atom]},
                   optic:get([optic:merge([id()])],
                             atom)),
     ?_assertEqual({ok, [atom, atom]},
                   optic:get([optic:merge([id(), id()])],
                             atom)),
     ?_assertEqual({ok, [atom, atom, atom]},
                   optic:get([optic:merge([id(), id(), id()])],
                             atom)),
     ?_assertEqual({ok, modified},
                   optic:put([optic:merge([id()])],
                             modified,
                             atom)),
     ?_assertEqual({ok, modified},
                   optic:put([optic:merge([id(), id()])],
                             modified,
                             atom)),
     ?_assertEqual({ok, modified},
                   optic:put([optic:merge([id(), id(), id()])],
                             modified,
                             atom))].

is_optic_test_() ->
    [?_assert(optic:is_optic(id())),
     ?_assert(not optic:is_optic({}))].

variations_test_() ->
    New = fun (_Data, _Template) -> undefined end,
    True = fun true/1,
    [?_assert(optic:is_optic(
                optic:variations(id(), #{}, New))),
     ?_assert(optic:is_optic(
                optic:variations(id(), #{strict=>false}, New))),
     ?_assert(optic:is_optic(
                optic:variations(id(), #{strict=>true}, New))),
     ?_assert(optic:is_optic(
                optic:variations(id(), #{create=>undefined}, New))),
     ?_assert(optic:is_optic(
                optic:variations(id(), #{filter=>fun true/1}, New))),
     ?_assert(optic:is_optic(
                optic:variations(id(), #{require=>fun true/1}, New))),
     ?_assertEqual(optic:variations(id(), #{}, New),
                   optic:variations(id(), #{strict=>false}, New)),
     ?_assertEqual(optic:variations(id(), [], New),
                   optic:variations(id(), #{strict=>false}, New)),
     ?_assertEqual(optic:variations(id(), [strict], New),
                   optic:variations(id(), #{strict=>true}, New)),
     ?_assertEqual(optic:variations(id(), [create], New),
                   optic:variations(id(), #{create=>true}, New)),
     ?_assertEqual(optic:variations(id(), [{filter, True}], New),
                   optic:variations(id(), #{filter=>True}, New)),
     ?_assertEqual(optic:variations(id(), [{require, True}], New),
                   optic:variations(id(), #{require=>True}, New))].

create_test_() ->
    Create = optic:create(
               optic:require(fun erlang:is_list/1),
               fun (_Data, _Template) -> [] end,
               template),
    [?_assertEqual({ok, [[]]},
                   optic:get(Create, [])),
     ?_assertEqual({error, required},
                   optic:get(Create, atom)),
     ?_assertEqual({ok, modified},
                   optic:put(Create, modified, [])),
     ?_assertEqual({ok, modified},
                   optic:put(Create, modified, atom))].

lax_test_() ->
    Lax = optic:lax(optic:require(fun erlang:is_list/1)),
    [?_assertEqual({ok, [[]]},
                   optic:get(Lax, [])),
     ?_assertEqual({ok, []},
                   optic:get(Lax, atom)),
     ?_assertEqual({ok, modified},
                   optic:put(Lax, modified, [])),
     ?_assertEqual({ok, atom},
                   optic:put(Lax, modified, atom))].

fold_test() ->
    ?assertEqual({ok, 2},
                 optic:fold([id()],
                            fun (atom, Acc) -> Acc + 1 end,
                            1,
                            atom)).

get_test() ->
    ?assertEqual({ok, [atom]},
                 optic:get([id()], atom)).

mapfold_test() ->
    ?assertEqual({ok, {modified, 2}},
                 optic:mapfold([id()],
                               fun (atom, Acc) ->
                                       {modified, Acc + 1}
                               end,
                               1,
                               atom)).

map_test() ->
    ?assertEqual({ok, modified},
                 optic:map([id()],
                           fun (atom) -> modified end,
                           atom)).

put_test() ->
    ?assertEqual({ok, modified},
                 optic:put([id()], modified, atom)).

id_get_test() ->
    ?assertEqual({ok, [atom]},
                 optic:get(optic:id(), atom)).

id_put_test() ->
    ?assertEqual({ok, new},
                 optic:put(optic:id(), new, atom)).

error_get_test() ->
    ?assertEqual({error, reason},
                 optic:get(optic:error(reason), atom)).

error_put_test() ->
    ?assertEqual({error, reason},
                 optic:put(optic:error(reason), new, atom)).

filter_get_test_() ->
    [?_assertEqual({ok, [atom]},
                   optic:get(optic:filter(fun true/1), atom)),
     ?_assertEqual({ok, []},
                   optic:get(optic:filter(fun false/1), atom))].

filter_put_test_() ->
    [?_assertEqual({ok, modified},
                   optic:put(optic:filter(fun true/1), modified, atom)),
     ?_assertEqual({ok, atom},
                   optic:put(optic:filter(fun false/1), modified, atom))].

require_get_test_() ->
    [?_assertEqual({ok, [atom]},
                   optic:get(optic:require(fun true/1), atom)),
     ?_assertEqual({error, required},
                   optic:get(optic:require(fun false/1), atom))].

require_put_test_() ->
    [?_assertEqual({ok, modified},
                   optic:put(optic:require(fun true/1), modified, atom)),
     ?_assertEqual({error, required},
                   optic:put(optic:require(fun false/1), modified, atom))].

%%%===================================================================
%%% Internal Functions
%%%===================================================================

id() ->
    MapFold = fun (Fun, Acc, Data) ->
                      {ok, Fun(Data, Acc)}
              end,
    optic:new(MapFold).

true(_) ->
    true.

false(_) ->
    false.
