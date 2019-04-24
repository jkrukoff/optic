%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for src/optic_gb_trees.erl
%%% @end
%%%-------------------------------------------------------------------
-module(test_optic_gb_trees).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

all_get_test_() ->
    [?_assertEqual({ok, [1, 2, 3]},
                   sort_get(optic:get([optic_gb_trees:all([strict])],
                                      from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_gb_trees:all([strict])], atom))].

all_put_test_() ->
    [?_assertEqual({ok, [{one, 4}, {three, 4}, {two, 4}]},
                   sort_put(optic:put([optic_gb_trees:all([strict])],
                                      from_list([{one, 1}, {two, 2}, {three, 3}]),
                                      4))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_gb_trees:all([strict])], atom, 4))].

all_create_test() ->
    ?assertEqual({ok, []},
                 sort_put(optic:put([optic_gb_trees:all([strict, create])], atom, 4))).

keys_get_test_() ->
    [?_assertEqual({ok, [one, three, two]},
                   sort_get(optic:get([optic_gb_trees:keys([strict])],
                                      from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_gb_trees:keys([strict])], atom))].

keys_put_test_() ->
    [?_assertEqual({ok, [{four, 1}]},
                   sort_put(optic:put([optic_gb_trees:keys([strict])],
                                      from_list([{one, 1}]),
                                      four))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_gb_trees:keys([strict])], atom, four))].

keys_create_test() ->
    ?assertEqual({ok, []},
                 sort_put(optic:put([optic_gb_trees:keys([strict, create])], atom, four))).

values_get_test_() ->
    [?_assertEqual({ok, [1, 2, 3]},
                   sort_get(optic:get([optic_gb_trees:values([strict])],
                                      from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_gb_trees:values([strict])], atom))].

values_put_test_() ->
    [?_assertEqual({ok, [{one, 4}, {three, 4}, {two, 4}]},
                   sort_put(optic:put([optic_gb_trees:values([strict])],
                                      from_list([{one, 1}, {two, 2}, {three, 3}]),
                                      4))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_gb_trees:values([strict])], atom, 4))].

values_create_test() ->
    ?assertEqual({ok, []},
                 sort_put(optic:put([optic_gb_trees:values([strict, create])], atom, 4))).

associations_get_test_() ->
    [?_assertEqual({ok, [{one, 1}, {three, 3}, {two, 2}]},
                   sort_get(optic:get([optic_gb_trees:associations([strict])],
                                      from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_gb_trees:associations([strict])], atom))].

associations_put_test_() ->
    [?_assertEqual({ok, [{four, 4}]},
                   sort_put(optic:put([optic_gb_trees:associations([strict])],
                                      from_list([{one, 1}]),
                                      {four, 4}))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_gb_trees:associations([strict])],
                             atom,
                             {four, 4}))].

associations_create_test() ->
    ?assertEqual({ok, []},
                 sort_put(optic:put([optic_gb_trees:associations([strict, create])],
                                    atom,
                                    {four, 4}))).

key_get_test_() ->
    [?_assertEqual({ok, [1]},
                   sort_get(optic:get([optic_gb_trees:key(one, [strict])],
                                      from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_gb_trees:key(one, [strict])], atom))].

key_put_test_() ->
    [?_assertEqual({ok, [{one, 4}, {three, 3}, {two, 2}]},
                   sort_put(optic:put([optic_gb_trees:key(one, [strict])],
                                      from_list([{one, 1}, {two, 2}, {three, 3}]),
                                      4))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_gb_trees:key(one, [strict])], atom, 4))].

key_create_test_() ->
    [?_assertEqual({ok, [{four, 4}, {one, 1}]},
                   sort_put(optic:put([optic_gb_trees:key(four, [strict, create])],
                                      from_list([{one, 1}]),
                                      4))),
     ?_assertEqual({ok, [{four, 4}]},
                   sort_put(optic:put([optic_gb_trees:key(four, [strict, create])],
                                      atom,
                                      4)))].

association_get_test_() ->
    [?_assertEqual({ok, [{one, 1}]},
                   sort_get(optic:get([optic_gb_trees:association(one, [strict])],
                                      from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_gb_trees:association(one, [strict])], atom))].

association_put_test_() ->
    [?_assertEqual({ok, [{four, 4}, {three, 3}, {two, 2}]},
                   sort_put(optic:put([optic_gb_trees:association(one, [strict])],
                                      from_list([{one, 1}, {two, 2}, {three, 3}]),
                                      {four, 4}))),
     ?_assertEqual({ok, [{four, 4}, {three, 3}, {two, 2}]},
                   sort_put(optic:put([optic_gb_trees:association(one, [strict])],
                                      from_list([{one, 1}, {two, 2}, {three, 3}, {four, clobbered}]),
                                      {four, 4}))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_gb_trees:association(one, [strict])],
                             atom,
                             4))].

association_create_test_() ->
    [?_assertEqual({ok, [{four, 4}, {one, 1}]},
                   sort_put(optic:put([optic_gb_trees:association(four, [strict, create])],
                                      from_list([{one, 1}]),
                                      {four, 4}))),
     ?_assertEqual({ok, [{four, 4}]},
                   sort_put(optic:put([optic_gb_trees:association(four, [strict, create])],
                                      atom,
                                      {four, 4})))].

%%%===================================================================
%%% Internal Functions
%%%===================================================================

from_list(List) ->
    Dict = orddict:from_list(List),
    gb_trees:from_orddict(Dict).

sort_get({ok, Result}) ->
    {ok, lists:sort(Result)}.

sort_put({ok, Result}) ->
    {ok, gb_trees:to_list(Result)}.
