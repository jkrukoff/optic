%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for src/optic_orddict.erl
%%% @end
%%%-------------------------------------------------------------------
-module(test_optic_orddict).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

all_get_test_() ->
    [?_assertEqual({ok, [1, 2, 3]},
                   sort_get(optic:get([optic_orddict:all([strict])],
                                      orddict:from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_orddict:all([strict])], atom))].

all_put_test_() ->
    [?_assertEqual({ok, orddict:from_list([{one, 4}, {two, 4}, {three, 4}])},
                   optic:put([optic_orddict:all([strict])],
                             4,
                             orddict:from_list([{one, 1}, {two, 2}, {three, 3}]))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_orddict:all([strict])], 4, atom))].

all_create_test() ->
    ?assertEqual({ok, orddict:new()},
                 optic:put([optic_orddict:all([strict, create])], 4, atom)).

keys_get_test_() ->
    [?_assertEqual({ok, [one, three, two]},
                   sort_get(optic:get([optic_orddict:keys([strict])],
                                      orddict:from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_orddict:keys([strict])], atom))].

keys_put_test_() ->
    [?_assertEqual({ok, orddict:from_list([{four, 1}])},
                   optic:put([optic_orddict:keys([strict])],
                             four,
                             orddict:from_list([{one, 1}]))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_orddict:keys([strict])], four, atom))].

keys_create_test() ->
    ?assertEqual({ok, orddict:new()},
                 optic:put([optic_orddict:keys([strict, create])], four, atom)).

values_get_test_() ->
    [?_assertEqual({ok, [1, 2, 3]},
                   sort_get(optic:get([optic_orddict:values([strict])],
                                      orddict:from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_orddict:values([strict])], atom))].

values_put_test_() ->
    [?_assertEqual({ok, orddict:from_list([{one, 4}, {two, 4}, {three, 4}])},
                   optic:put([optic_orddict:values([strict])],
                             4,
                             orddict:from_list([{one, 1}, {two, 2}, {three, 3}]))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_orddict:values([strict])], 4, atom))].

values_create_test() ->
    ?assertEqual({ok, orddict:new()},
                 optic:put([optic_orddict:values([strict, create])], 4, atom)).

associations_get_test_() ->
    [?_assertEqual({ok, [{one, 1}, {three, 3}, {two, 2}]},
                   sort_get(optic:get([optic_orddict:associations([strict])],
                                      orddict:from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_orddict:associations([strict])], atom))].

associations_put_test_() ->
    [?_assertEqual({ok, orddict:from_list([{four, 4}])},
                   optic:put([optic_orddict:associations([strict])],
                             {four, 4},
                             orddict:from_list([{one, 1}]))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_orddict:associations([strict])],
                             {four, 4},
                             atom))].

associations_create_test() ->
    ?assertEqual({ok, orddict:new()},
                 optic:put([optic_orddict:associations([strict, create])],
                           {four, 4},
                           atom)).

key_get_test_() ->
    [?_assertEqual({ok, [1]},
                   sort_get(optic:get([optic_orddict:key(one, [strict])],
                                      orddict:from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_orddict:key(one, [strict])], atom))].

key_put_test_() ->
    [?_assertEqual({ok, orddict:from_list([{one, 4}, {two, 2}, {three, 3}])},
                   optic:put([optic_orddict:key(one, [strict])],
                             4,
                             orddict:from_list([{one, 1}, {two, 2}, {three, 3}]))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_orddict:key(one, [strict])], 4, atom))].

key_create_test_() ->
    [?_assertEqual({ok, orddict:from_list([{one, 1}, {four, 4}])},
                   optic:put([optic_orddict:key(four, [strict, create])],
                             4,
                             orddict:from_list([{one, 1}]))),
     ?_assertEqual({ok, orddict:from_list([{four, 4}])},
                   optic:put([optic_orddict:key(four, [strict, create])],
                             4,
                             atom))].

association_get_test_() ->
    [?_assertEqual({ok, [{one, 1}]},
                   sort_get(optic:get([optic_orddict:association(one, [strict])],
                                      orddict:from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_orddict:association(one, [strict])], atom))].

association_put_test_() ->
    [?_assertEqual({ok, orddict:from_list([{two, 2}, {three, 3}, {four, 4}])},
                   optic:put([optic_orddict:association(one, [strict])],
                             {four, 4},
                             orddict:from_list([{one, 1}, {two, 2}, {three, 3}]))),
     ?_assertEqual({ok, orddict:from_list([{two, 2}, {three, 3}, {four, 4}])},
                   optic:put([optic_orddict:association(one, [strict])],
                             {four, 4},
                             orddict:from_list([{one, 1}, {two, 2}, {three, 3}, {four, clobbered}]))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_orddict:association(one, [strict])],
                             4,
                             atom))].

association_create_test_() ->
    [?_assertEqual({ok, orddict:from_list([{one, 1}, {four, 4}])},
                   optic:put([optic_orddict:association(four, [strict, create])],
                             {four, 4},
                             orddict:from_list([{one, 1}]))),
     ?_assertEqual({ok, orddict:from_list([{four, 4}])},
                   optic:put([optic_orddict:association(four, [strict, create])],
                             {four, 4},
                             atom))].

%%%===================================================================
%%% Internal Functions
%%%===================================================================

sort_get({ok, Result}) ->
    {ok, lists:sort(Result)}.
