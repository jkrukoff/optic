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
                   sort_get(
                     optic:get([optic_dict:all([strict])],
                               dict:from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_dict:all([strict])], atom))].

all_put_test_() ->
    [?_assertEqual({ok, dict:from_list([{one, 4}, {two, 4}, {three, 4}])},
                   optic:put([optic_dict:all([strict])],
                             4,
                             dict:from_list([{one, 1}, {two, 2}, {three, 3}]))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_dict:all([strict])], 4, atom))].

all_create_test() ->
    ?assertEqual({ok, dict:new()},
                 optic:put([optic_dict:all([strict, create])], 4, atom)).

keys_get_test_() ->
    [?_assertEqual({ok, [one, three, two]},
                   sort_get(
                     optic:get([optic_dict:keys([strict])],
                               dict:from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_dict:keys([strict])], atom))].

keys_put_test_() ->
    [?_assertEqual({ok, dict:from_list([{four, 1}])},
                   optic:put([optic_dict:keys([strict])],
                             four,
                             dict:from_list([{one, 1}]))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_dict:keys([strict])], four, atom))].

keys_create_test() ->
    ?assertEqual({ok, dict:new()},
                 optic:put([optic_dict:keys([strict, create])], four, atom)).

values_get_test_() ->
    [?_assertEqual({ok, [1, 2, 3]},
                   sort_get(
                     optic:get([optic_dict:values([strict])],
                               dict:from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_dict:values([strict])], atom))].

values_put_test_() ->
    [?_assertEqual({ok, dict:from_list([{one, 4}, {two, 4}, {three, 4}])},
                   optic:put([optic_dict:values([strict])],
                             4,
                             dict:from_list([{one, 1}, {two, 2}, {three, 3}]))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_dict:values([strict])], 4, atom))].

values_create_test() ->
    ?assertEqual({ok, dict:new()},
                 optic:put([optic_dict:values([strict, create])], 4, atom)).

associations_get_test_() ->
    [?_assertEqual({ok, [{one, 1}, {three, 3}, {two, 2}]},
                   sort_get(
                     optic:get([optic_dict:associations([strict])],
                               dict:from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_dict:associations([strict])], atom))].

associations_put_test_() ->
    [?_assertEqual({ok, dict:from_list([{four, 4}])},
                   optic:put([optic_dict:associations([strict])],
                             {four, 4},
                             dict:from_list([{one, 1}]))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_dict:associations([strict])],
                             {four, 4},
                             atom))].

associations_create_test() ->
    ?assertEqual({ok, dict:new()},
                 optic:put([optic_dict:associations([strict, create])],
                           {four, 4},
                           atom)).

key_get_test_() ->
    [?_assertEqual({ok, [1]},
                   sort_get(
                     optic:get([optic_dict:key(one, [strict])],
                               dict:from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_dict:key(one, [strict])], atom))].

key_put_test_() ->
    [?_assertEqual({ok, dict:from_list([{one, 4}, {two, 2}, {three, 3}])},
                   optic:put([optic_dict:key(one, [strict])],
                             4,
                             dict:from_list([{one, 1}, {two, 2}, {three, 3}]))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_dict:key(one, [strict])], 4, atom))].

key_create_test_() ->
    [?_assertEqual({ok, dict:from_list([{one, 1}, {four, 4}])},
                   optic:put([optic_dict:key(four, [strict, create])],
                             4,
                             dict:from_list([{one, 1}]))),
     ?_assertEqual({ok, dict:from_list([{four, 4}])},
                   optic:put([optic_dict:key(four, [strict, create])],
                             4,
                             atom))].

association_get_test_() ->
    [?_assertEqual({ok, [{one, 1}]},
                   sort_get(
                     optic:get([optic_dict:association(one, [strict])],
                               dict:from_list([{one, 1}, {two, 2}, {three, 3}])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_dict:association(one, [strict])], atom))].

association_put_test_() ->
    [?_assertEqual({ok, dict:from_list([{two, 2}, {three, 3}, {four, 4}])},
                   optic:put([optic_dict:association(one, [strict])],
                             {four, 4},
                             dict:from_list([{one, 1}, {two, 2}, {three, 3}]))),
     ?_assertEqual({ok, dict:from_list([{two, 2}, {three, 3}, {four, 4}])},
                   optic:put([optic_dict:association(one, [strict])],
                             {four, 4},
                             dict:from_list([{one, 1}, {two, 2}, {three, 3}, {four, clobbered}]))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_dict:association(one, [strict])],
                             4,
                             atom))].

association_create_test_() ->
    [?_assertEqual({ok, dict:from_list([{one, 1}, {four, 4}])},
                   optic:put([optic_dict:association(four, [strict, create])],
                             {four, 4},
                             dict:from_list([{one, 1}]))),
     ?_assertEqual({ok, dict:from_list([{four, 4}])},
                   optic:put([optic_dict:association(four, [strict, create])],
                             {four, 4},
                             atom))].

%%%===================================================================
%%% Internal Functions
%%%===================================================================

sort_get({ok, Result}) ->
    {ok, lists:sort(Result)}.
