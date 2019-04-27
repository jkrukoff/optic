%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for src/optic_generic.erl
%%% @end
%%%-------------------------------------------------------------------
-module(test_optic_generic).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

key_get_test_() ->
    [?_assertEqual({ok, [value]},
                   optic:get([optic_generic:key(key)], #{key=>value})),
     ?_assertEqual({ok, [value]},
                   optic:get([optic_generic:key(key)], [{key, value}])),
     ?_assertEqual({ok, [value]},
                   optic:get([optic_generic:key(key)], orddict:from_list([{key, value}]))),
     ?_assertEqual({ok, [value]},
                   optic:get([optic_generic:key(key)], dict:from_list([{key, value}]))),
     ?_assertEqual({ok, [value]},
                   optic:get([optic_generic:key(key)], gb_trees:from_orddict([{key, value}]))),
     ?_assertEqual({ok, []},
                   optic:get([optic_generic:key(key)], atom))].

key_put_test_() ->
    [?_assertEqual({ok, #{key=>new}},
                   optic:put([optic_generic:key(key)], new, #{key=>value})),
     ?_assertEqual({ok, [{key, new}]},
                   optic:put([optic_generic:key(key)], new, [{key, value}])),
     ?_assertEqual({ok, [{key, new}]},
                   optic:put([optic_generic:key(key)], new, orddict:from_list([{key, value}]))),
     ?_assertEqual({ok, dict:from_list([{key, new}])},
                   optic:put([optic_generic:key(key)], new, dict:from_list([{key, value}]))),
     ?_assertEqual({ok, gb_trees:from_orddict([{key, new}])},
                   optic:put([optic_generic:key(key)], new, gb_trees:from_orddict([{key, value}]))),
     ?_assertEqual({ok, atom},
                   optic:put([optic_generic:key(key)], new, atom))].

index_get_test_() ->
    [?_assertEqual({ok, [3]},
                   optic:get([optic_generic:index(3)], [1, 2, 3])),
     ?_assertEqual({ok, [3]},
                   optic:get([optic_generic:index(3)], {1, 2, 3})),
     ?_assertEqual({ok, []},
                   optic:get([optic_generic:index(3)], #{}))].

index_put_test_() ->
    [?_assertEqual({ok, [1, 2, 4]},
                   optic:put([optic_generic:index(3)], 4, [1, 2, 3])),
     ?_assertEqual({ok, {1, 2, 4}},
                   optic:put([optic_generic:index(3)], 4, {1, 2, 3})),
     ?_assertEqual({ok, #{}},
                   optic:put([optic_generic:index(3)], 4, #{}))].
