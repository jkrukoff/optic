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
                   sort_get(optic:get([optic_maps:all()],
                                      #{one=>1, two=>2, three=>3}))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_maps:all()], atom))].

all_put_test_() ->
    [?_assertEqual({ok, #{one=>4, two=>4, three=>4}},
                   optic:put([optic_maps:all()],
                             #{one=>1, two=>2, three=>3},
                             4)),
     ?_assertEqual({error, undefined},
                   optic:put([optic_maps:all()], atom, 4))].

all_create_test() ->
    ?assertEqual({ok, #{}},
                 optic:put([optic_maps:all([create])], atom, 4)).

keys_get_test_() ->
    [?_assertEqual({ok, [one, three, two]},
                   sort_get(optic:get([optic_maps:keys()],
                                      #{one=>1, two=>2, three=>3}))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_maps:keys()], atom))].

keys_put_test_() ->
    [?_assertEqual({ok, #{four=>1}},
                   optic:put([optic_maps:keys()],
                             #{one=>1},
                             four)),
     ?_assertEqual({error, undefined},
                   optic:put([optic_maps:keys()], atom, four))].

keys_create_test() ->
    ?assertEqual({ok, #{}},
                 optic:put([optic_maps:keys([create])], atom, four)).

values_get_test_() ->
    [?_assertEqual({ok, [1, 2, 3]},
                   sort_get(optic:get([optic_maps:values()],
                                      #{one=>1, two=>2, three=>3}))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_maps:values()], atom))].

values_put_test_() ->
    [?_assertEqual({ok, #{one=>4, two=>4, three=>4}},
                   optic:put([optic_maps:values()],
                             #{one=>1, two=>2, three=>3},
                             4)),
     ?_assertEqual({error, undefined},
                   optic:put([optic_maps:values()], atom, 4))].

values_create_test() ->
    ?assertEqual({ok, #{}},
                 optic:put([optic_maps:values([create])], atom, 4)).

associations_get_test_() ->
    [?_assertEqual({ok, [{one, 1}, {three, 3}, {two, 2}]},
                   sort_get(optic:get([optic_maps:associations()],
                                      #{one=>1, two=>2, three=>3}))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_maps:associations()], atom))].

associations_put_test_() ->
    [?_assertEqual({ok, #{four=>4}},
                   optic:put([optic_maps:associations()],
                             #{one=>1},
                             {four, 4})),
     ?_assertEqual({error, undefined},
                   optic:put([optic_maps:associations()], atom, {four, 4}))].

associations_create_test() ->
    ?assertEqual({ok, #{}},
                 optic:put([optic_maps:associations([create])], atom, {four, 4})).

key_get_test_() ->
    [?_assertEqual({ok, [1]},
                   sort_get(optic:get([optic_maps:key(one)],
                                      #{one=>1, two=>2, three=>3}))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_maps:key(one)], atom))].

key_put_test_() ->
    [?_assertEqual({ok, #{one=>4, two=>2, three=>3}},
                   optic:put([optic_maps:key(one)],
                             #{one=>1, two=>2, three=>3},
                             4)),
     ?_assertEqual({error, undefined},
                   optic:put([optic_maps:key(one)], atom, 4))].

key_create_test_() ->
    [?_assertEqual({ok, #{one=>1, four=>4}},
                   optic:put([optic_maps:key(four, [create])], #{one=>1}, 4)),
     ?_assertEqual({ok, #{four=>4}},
                   optic:put([optic_maps:key(four, [create])], atom, 4))].

association_get_test_() ->
    [?_assertEqual({ok, [{one, 1}]},
                   sort_get(optic:get([optic_maps:association(one)],
                                      #{one=>1, two=>2, three=>3}))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_maps:association(one)], atom))].

association_put_test_() ->
    [?_assertEqual({ok, #{two=>2, three=>3, four=>4}},
                   optic:put([optic_maps:association(one)],
                             #{one=>1, two=>2, three=>3},
                             {four, 4})),
     ?_assertEqual({ok, #{two=>2, three=>3, four=>4}},
                   optic:put([optic_maps:association(one)],
                             #{one=>1, two=>2, three=>3, four=>clobbered},
                             {four, 4})),
     ?_assertEqual({error, undefined},
                   optic:put([optic_maps:association(one)], atom, 4))].

association_create_test_() ->
    [?_assertEqual({ok, #{one=>1, four=>4}},
                   optic:put([optic_maps:association(four, [create])],
                             #{one=>1},
                             {four, 4})),
     ?_assertEqual({ok, #{four=>4}},
                   optic:put([optic_maps:association(four, [create])],
                             atom,
                             {four, 4}))].

%%%===================================================================
%%% Internal Functions
%%%===================================================================

sort_get({ok, Result}) ->
    {ok, lists:sort(Result)}.
