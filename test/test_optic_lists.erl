%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for src/optic_lists.erl
%%% @end
%%%-------------------------------------------------------------------
-module(test_optic_lists).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

all_get_test_() ->
    [?_assertEqual({ok, [1, 2, 3]},
                   optic:get([optic_lists:all()], [1, 2, 3])),
     ?_assertEqual({error, undefined},
                   optic:get([optic_lists:all()], atom))].

all_put_test_() ->
    [?_assertEqual({ok, [4, 4, 4]},
                   optic:put([optic_lists:all()], [1, 2, 3], 4)),
     ?_assertEqual({error, undefined},
                   optic:put([optic_lists:all()], atom, 4))].

all_create_test() ->
    ?assertEqual({ok, []},
                 optic:put([optic_lists:all([create])], atom, 4)).

head_get_test_() ->
    [?_assertEqual({ok, [1]},
                   optic:get([optic_lists:head()], [1, 2, 3])),
     ?_assertEqual({error, undefined},
                   optic:get([optic_lists:head()], atom))].

head_put_test_() ->
    [?_assertEqual({ok, [4, 2, 3]},
                   optic:put([optic_lists:head()], [1, 2, 3], 4)),
     ?_assertEqual({error, undefined},
                   optic:put([optic_lists:head()], atom, 4))].

head_create_test() ->
    ?assertEqual({ok, [4]},
                 optic:put([optic_lists:head([create])], atom, 4)).

tail_get_test_() ->
    [?_assertEqual({ok, [2, 3]},
                   optic:get([optic_lists:tail()], [1, 2, 3])),
     ?_assertEqual({error, undefined},
                   optic:get([optic_lists:tail()], atom))].

tail_put_test_() ->
    [?_assertEqual({ok, [1, 4, 4]},
                   optic:put([optic_lists:tail()], [1, 2, 3], 4)),
     ?_assertEqual({error, undefined},
                   optic:put([optic_lists:tail()], atom, 4))].

tail_create_test() ->
    ?assertEqual({ok, [undefined]},
                 optic:put([optic_lists:tail([{create, undefined}])], atom, 4)).

nth_get_test_() ->
    [?_assertEqual({ok, [1]},
                   optic:get([optic_lists:nth(1)], [1, 2, 3])),
     ?_assertEqual({error, undefined},
                   optic:get([optic_lists:nth(1)], atom))].

nth_put_test_() ->
    [?_assertEqual({ok, [4, 2, 3]},
                   optic:put([optic_lists:nth(1)], [1, 2, 3], 4)),
     ?_assertEqual({error, undefined},
                   optic:put([optic_lists:nth(1)], atom, 4))].

nth_create_test_() ->
    [?_assertEqual({ok, [undefined, 4]},
                   optic:put([optic_lists:nth(2, [{create, undefined}])], atom, 4)),
     ?_assertEqual({ok, [1, undefined, 4]},
                   optic:put([optic_lists:nth(3, [{create, undefined}])], [1], 4))].
