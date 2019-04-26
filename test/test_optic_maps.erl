%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for src/optic_maps.erl
%%% @end
%%%-------------------------------------------------------------------
-module(test_optic_maps).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

all_get_test_() ->
    [?_assertEqual({ok, [1, 2, 3]},
                   sort_get(optic:get([optic_maps:all([strict])],
                                      #{one=>1, two=>2, three=>3}))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_maps:all([strict])], atom))].

all_put_test_() ->
    [?_assertEqual({ok, #{one=>4, two=>4, three=>4}},
                   optic:put([optic_maps:all([strict])],
                             4,
                             #{one=>1, two=>2, three=>3})),
     ?_assertEqual({error, undefined},
                   optic:put([optic_maps:all([strict])], 4, atom))].

all_create_test() ->
    ?assertEqual({ok, #{}},
                 optic:put([optic_maps:all([strict, create])], 4, atom)).

keys_get_test_() ->
    [?_assertEqual({ok, [one, three, two]},
                   sort_get(optic:get([optic_maps:keys([strict])],
                                      #{one=>1, two=>2, three=>3}))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_maps:keys([strict])], atom))].

keys_put_test_() ->
    [?_assertEqual({ok, #{four=>1}},
                   optic:put([optic_maps:keys([strict])],
                             four,
                             #{one=>1})),
     ?_assertEqual({error, undefined},
                   optic:put([optic_maps:keys([strict])], four, atom))].

keys_create_test() ->
    ?assertEqual({ok, #{}},
                 optic:put([optic_maps:keys([strict, create])], four, atom)).

values_get_test_() ->
    [?_assertEqual({ok, [1, 2, 3]},
                   sort_get(optic:get([optic_maps:values([strict])],
                                      #{one=>1, two=>2, three=>3}))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_maps:values([strict])], atom))].

values_put_test_() ->
    [?_assertEqual({ok, #{one=>4, two=>4, three=>4}},
                   optic:put([optic_maps:values([strict])],
                             4,
                             #{one=>1, two=>2, three=>3})),
     ?_assertEqual({error, undefined},
                   optic:put([optic_maps:values([strict])], 4, atom))].

values_create_test() ->
    ?assertEqual({ok, #{}},
                 optic:put([optic_maps:values([strict, create])], 4, atom)).

associations_get_test_() ->
    [?_assertEqual({ok, [{one, 1}, {three, 3}, {two, 2}]},
                   sort_get(optic:get([optic_maps:associations([strict])],
                                      #{one=>1, two=>2, three=>3}))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_maps:associations([strict])], atom))].

associations_put_test_() ->
    [?_assertEqual({ok, #{four=>4}},
                   optic:put([optic_maps:associations([strict])],
                             {four, 4},
                             #{one=>1})),
     ?_assertEqual({error, undefined},
                   optic:put([optic_maps:associations([strict])],
                             {four, 4},
                             atom))].

associations_create_test() ->
    ?assertEqual({ok, #{}},
                 optic:put([optic_maps:associations([strict, create])],
                           {four, 4},
                           atom)).

key_get_test_() ->
    [?_assertEqual({ok, [1]},
                   sort_get(optic:get([optic_maps:key(one, [strict])],
                                      #{one=>1, two=>2, three=>3}))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_maps:key(one, [strict])], atom))].

key_put_test_() ->
    [?_assertEqual({ok, #{one=>4, two=>2, three=>3}},
                   optic:put([optic_maps:key(one, [strict])],
                             4,
                             #{one=>1, two=>2, three=>3})),
     ?_assertEqual({error, undefined},
                   optic:put([optic_maps:key(one, [strict])], 4, atom))].

key_create_test_() ->
    [?_assertEqual({ok, #{one=>1, four=>4}},
                   optic:put([optic_maps:key(four, [strict, create])],
                             4,
                             #{one=>1})),
     ?_assertEqual({ok, #{four=>4}},
                   optic:put([optic_maps:key(four, [strict, create])],
                             4,
                             atom))].

association_get_test_() ->
    [?_assertEqual({ok, [{one, 1}]},
                   sort_get(optic:get([optic_maps:association(one, [strict])],
                                      #{one=>1, two=>2, three=>3}))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_maps:association(one, [strict])], atom))].

association_put_test_() ->
    [?_assertEqual({ok, #{two=>2, three=>3, four=>4}},
                   optic:put([optic_maps:association(one, [strict])],
                             {four, 4},
                             #{one=>1, two=>2, three=>3})),
     ?_assertEqual({ok, #{two=>2, three=>3, four=>4}},
                   optic:put([optic_maps:association(one, [strict])],
                             {four, 4},
                             #{one=>1, two=>2, three=>3, four=>clobbered})),
     ?_assertEqual({error, undefined},
                   optic:put([optic_maps:association(one, [strict])],
                             4,
                             atom))].

association_create_test_() ->
    [?_assertEqual({ok, #{one=>1, four=>4}},
                   optic:put([optic_maps:association(four, [strict, create])],
                             {four, 4},
                             #{one=>1})),
     ?_assertEqual({ok, #{four=>4}},
                   optic:put([optic_maps:association(four, [strict, create])],
                             {four, 4},
                             atom))].

%%%===================================================================
%%% Internal Functions
%%%===================================================================

sort_get({ok, Result}) ->
    {ok, lists:sort(Result)}.
