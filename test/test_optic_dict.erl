%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for src/optic_dict.erl
%%% @end
%%%-------------------------------------------------------------------
-module(test_optic_dict).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

all_get_test_() ->
    [?_assertEqual({ok, [1, 2, 3]},
                   sort_get(optic:get([optic_dict:all([strict])],
                                      dict:from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_dict:all([strict])], atom))].

all_put_test_() ->
    [?_assertEqual({ok, dict:from_list([{one, 4}, {two, 4}, {three, 4}])},
                   optic:put([optic_dict:all([strict])],
                             dict:from_list([{one, 1}, {two, 2}, {three, 3}]),
                             4)),
     ?_assertEqual({error, undefined},
                   optic:put([optic_dict:all([strict])], atom, 4))].

all_create_test() ->
    ?assertEqual({ok, dict:new()},
                 optic:put([optic_dict:all([strict, create])], atom, 4)).

keys_get_test_() ->
    [?_assertEqual({ok, [one, three, two]},
                   sort_get(optic:get([optic_dict:keys([strict])],
                                      dict:from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_dict:keys([strict])], atom))].

keys_put_test_() ->
    [?_assertEqual({ok, dict:from_list([{four, 1}])},
                   optic:put([optic_dict:keys([strict])],
                             dict:from_list([{one, 1}]),
                             four)),
     ?_assertEqual({error, undefined},
                   optic:put([optic_dict:keys([strict])], atom, four))].

keys_create_test() ->
    ?assertEqual({ok, dict:new()},
                 optic:put([optic_dict:keys([strict, create])], atom, four)).

values_get_test_() ->
    [?_assertEqual({ok, [1, 2, 3]},
                   sort_get(optic:get([optic_dict:values([strict])],
                                      dict:from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_dict:values([strict])], atom))].

values_put_test_() ->
    [?_assertEqual({ok, dict:from_list([{one, 4}, {two, 4}, {three, 4}])},
                   optic:put([optic_dict:values([strict])],
                             dict:from_list([{one, 1}, {two, 2}, {three, 3}]),
                             4)),
     ?_assertEqual({error, undefined},
                   optic:put([optic_dict:values([strict])], atom, 4))].

values_create_test() ->
    ?assertEqual({ok, dict:new()},
                 optic:put([optic_dict:values([strict, create])], atom, 4)).

associations_get_test_() ->
    [?_assertEqual({ok, [{one, 1}, {three, 3}, {two, 2}]},
                   sort_get(optic:get([optic_dict:associations([strict])],
                                      dict:from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_dict:associations([strict])], atom))].

associations_put_test_() ->
    [?_assertEqual({ok, dict:from_list([{four, 4}])},
                   optic:put([optic_dict:associations([strict])],
                             dict:from_list([{one, 1}]),
                             {four, 4})),
     ?_assertEqual({error, undefined},
                   optic:put([optic_dict:associations([strict])],
                             atom,
                             {four, 4}))].

associations_create_test() ->
    ?assertEqual({ok, dict:new()},
                 optic:put([optic_dict:associations([strict, create])],
                           atom,
                           {four, 4})).

key_get_test_() ->
    [?_assertEqual({ok, [1]},
                   sort_get(optic:get([optic_dict:key(one, [strict])],
                                      dict:from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_dict:key(one, [strict])], atom))].

key_put_test_() ->
    [?_assertEqual({ok, dict:from_list([{one, 4}, {two, 2}, {three, 3}])},
                   optic:put([optic_dict:key(one, [strict])],
                             dict:from_list([{one, 1}, {two, 2}, {three, 3}]),
                             4)),
     ?_assertEqual({error, undefined},
                   optic:put([optic_dict:key(one, [strict])], atom, 4))].

key_create_test_() ->
    [?_assertEqual({ok, dict:from_list([{one, 1}, {four, 4}])},
                   optic:put([optic_dict:key(four, [strict, create])],
                             dict:from_list([{one, 1}]),
                             4)),
     ?_assertEqual({ok, dict:from_list([{four, 4}])},
                   optic:put([optic_dict:key(four, [strict, create])],
                             atom,
                             4))].

association_get_test_() ->
    [?_assertEqual({ok, [{one, 1}]},
                   sort_get(optic:get([optic_dict:association(one, [strict])],
                                      dict:from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_dict:association(one, [strict])], atom))].

association_put_test_() ->
    [?_assertEqual({ok, dict:from_list([{two, 2}, {three, 3}, {four, 4}])},
                   optic:put([optic_dict:association(one, [strict])],
                             dict:from_list([{one, 1}, {two, 2}, {three, 3}]),
                             {four, 4})),
     ?_assertEqual({ok, dict:from_list([{two, 2}, {three, 3}, {four, 4}])},
                   optic:put([optic_dict:association(one, [strict])],
                             dict:from_list([{one, 1}, {two, 2}, {three, 3}, {four, clobbered}]),
                             {four, 4})),
     ?_assertEqual({error, undefined},
                   optic:put([optic_dict:association(one, [strict])],
                             atom,
                             4))].

association_create_test_() ->
    [?_assertEqual({ok, dict:from_list([{one, 1}, {four, 4}])},
                   optic:put([optic_dict:association(four, [strict, create])],
                             dict:from_list([{one, 1}]),
                             {four, 4})),
     ?_assertEqual({ok, dict:from_list([{four, 4}])},
                   optic:put([optic_dict:association(four, [strict, create])],
                             atom,
                             {four, 4}))].

%%%===================================================================
%%% Internal Functions
%%%===================================================================

sort_get({ok, Result}) ->
    {ok, lists:sort(Result)}.
