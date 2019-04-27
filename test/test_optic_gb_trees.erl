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
                   sort_get(
                     optic:get([optic_gb_trees:all([strict])],
                               from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_gb_trees:all([strict])], atom))].

all_put_test_() ->
    [?_assertEqual({ok, [{one, 4}, {three, 4}, {two, 4}]},
                   sort_put(
                     optic:put([optic_gb_trees:all([strict])],
                               4,
                               from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_gb_trees:all([strict])], 4, atom))].

all_create_test() ->
    ?assertEqual({ok, []},
                 sort_put(
                   optic:put([optic_gb_trees:all([strict, create])], 4, atom))).

keys_get_test_() ->
    [?_assertEqual({ok, [one, three, two]},
                   sort_get(
                     optic:get([optic_gb_trees:keys([strict])],
                               from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_gb_trees:keys([strict])], atom))].

keys_put_test_() ->
    [?_assertEqual({ok, [{four, 1}]},
                   sort_put(
                     optic:put([optic_gb_trees:keys([strict])],
                               four,
                               from_list([{one, 1}])))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_gb_trees:keys([strict])], four, atom))].

keys_create_test() ->
    ?assertEqual({ok, []},
                 sort_put(
                   optic:put([optic_gb_trees:keys([strict, create])], four, atom))).

values_get_test_() ->
    [?_assertEqual({ok, [1, 2, 3]},
                   sort_get(
                     optic:get([optic_gb_trees:values([strict])],
                               from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_gb_trees:values([strict])], atom))].

values_put_test_() ->
    [?_assertEqual({ok, [{one, 4}, {three, 4}, {two, 4}]},
                   sort_put(
                     optic:put([optic_gb_trees:values([strict])],
                               4,
                               from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_gb_trees:values([strict])], 4, atom))].

values_create_test() ->
    ?assertEqual({ok, []},
                 sort_put(
                   optic:put([optic_gb_trees:values([strict, create])], 4, atom))).

associations_get_test_() ->
    [?_assertEqual({ok, [{one, 1}, {three, 3}, {two, 2}]},
                   sort_get(
                     optic:get([optic_gb_trees:associations([strict])],
                               from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_gb_trees:associations([strict])], atom))].

associations_put_test_() ->
    [?_assertEqual({ok, [{four, 4}]},
                   sort_put(
                     optic:put([optic_gb_trees:associations([strict])],
                               {four, 4},
                               from_list([{one, 1}])))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_gb_trees:associations([strict])],
                             {four, 4},
                             atom))].

associations_create_test() ->
    ?assertEqual({ok, []},
                 sort_put(
                   optic:put([optic_gb_trees:associations([strict, create])],
                             {four, 4},
                             atom))).

key_get_test_() ->
    [?_assertEqual({ok, [1]},
                   sort_get(
                     optic:get([optic_gb_trees:key(one, [strict])],
                               from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_gb_trees:key(one, [strict])], atom))].

key_put_test_() ->
    [?_assertEqual({ok, [{one, 4}, {three, 3}, {two, 2}]},
                   sort_put(
                     optic:put([optic_gb_trees:key(one, [strict])],
                               4,
                               from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_gb_trees:key(one, [strict])], 4, atom))].

key_create_test_() ->
    [?_assertEqual({ok, [{four, 4}, {one, 1}]},
                   sort_put(
                     optic:put([optic_gb_trees:key(four, [strict, create])],
                               4,
                               from_list([{one, 1}])))),
     ?_assertEqual({ok, [{four, 4}]},
                   sort_put(
                     optic:put([optic_gb_trees:key(four, [strict, create])],
                               4,
                               atom)))].

association_get_test_() ->
    [?_assertEqual({ok, [{one, 1}]},
                   sort_get(
                     optic:get([optic_gb_trees:association(one, [strict])],
                               from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_gb_trees:association(one, [strict])], atom))].

association_put_test_() ->
    [?_assertEqual({ok, [{four, 4}, {three, 3}, {two, 2}]},
                   sort_put(
                     optic:put([optic_gb_trees:association(one, [strict])],
                               {four, 4},
                               from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({ok, [{four, 4}, {three, 3}, {two, 2}]},
                   sort_put(
                     optic:put([optic_gb_trees:association(one, [strict])],
                               {four, 4},
                               from_list([{one, 1}, {two, 2}, {three, 3}, {four, clobbered}])))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_gb_trees:association(one, [strict])],
                             4,
                             atom))].

association_create_test_() ->
    [?_assertEqual({ok, [{four, 4}, {one, 1}]},
                   sort_put(
                     optic:put([optic_gb_trees:association(four, [strict, create])],
                               {four, 4},
                               from_list([{one, 1}])))),
     ?_assertEqual({ok, [{four, 4}]},
                   sort_put(
                     optic:put([optic_gb_trees:association(four, [strict, create])],
                               {four, 4},
                               atom)))].

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
