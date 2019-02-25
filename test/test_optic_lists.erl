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
                   optic:get([optic_lists:all([strict])], [1, 2, 3])),
     ?_assertEqual({ok, [3]},
                   optic:get([optic_lists:all([strict,
                                               {filter,
                                                fun (Elem) -> Elem == 3 end}])],
                             [1, 2, 3])),
     ?_assertEqual({ok, [3]},
                   optic:get([optic_lists:all([strict,
                                               {require,
                                                fun (Elem) -> Elem == 3 end}])],
                             [3])),
     ?_assertEqual({error, required},
                   optic:get([optic_lists:all([create,
                                               {require,
                                                fun (Elem) -> Elem == 3 end}])],
                             [1, 2, 3])),
     ?_assertEqual({ok, []},
                   optic:get([optic_lists:all()], atom)),
     ?_assertEqual({ok, []},
                   optic:get([optic_lists:all([{strict, false}])], atom)),
     ?_assertEqual({error, undefined},
                   optic:get([optic_lists:all([strict])], atom))].

all_put_test_() ->
    [?_assertEqual({ok, [4, 4, 4]},
                   optic:put([optic_lists:all([strict])], [1, 2, 3], 4)),
     ?_assertEqual({ok, [1, 2, 4]},
                   optic:put([optic_lists:all([strict,
                                               {filter,
                                                fun (Elem) -> Elem == 3 end}])],
                             [1, 2, 3],
                             4)),
     ?_assertEqual({ok, [4]},
                   optic:put([optic_lists:all([strict,
                                               {require,
                                                fun (Elem) -> Elem == 3 end}])],
                             [3],
                             4)),
     ?_assertEqual({error, required},
                   optic:put([optic_lists:all([create,
                                               {require,
                                                fun (Elem) -> Elem == 3 end}])],
                             [1, 2, 3],
                             4)),
     ?_assertEqual({ok, atom},
                   optic:put([optic_lists:all()], atom, 4)),
     ?_assertEqual({ok, atom},
                   optic:put([optic_lists:all([{strict, false}])], atom, 4)),
     ?_assertEqual({error, undefined},
                   optic:put([optic_lists:all([strict])], atom, 4))].

all_create_test() ->
    ?assertEqual({ok, []},
                 optic:put([optic_lists:all([strict, create])], atom, 4)).

head_get_test_() ->
    [?_assertEqual({ok, [1]},
                   optic:get([optic_lists:head([strict])], [1, 2, 3])),
     ?_assertEqual({error, undefined},
                   optic:get([optic_lists:head([strict])], atom))].

head_put_test_() ->
    [?_assertEqual({ok, [4, 2, 3]},
                   optic:put([optic_lists:head([strict])], [1, 2, 3], 4)),
     ?_assertEqual({error, undefined},
                   optic:put([optic_lists:head([strict])], atom, 4))].

head_create_test() ->
    ?assertEqual({ok, [4]},
                 optic:put([optic_lists:head([strict, create])], atom, 4)).

tail_get_test_() ->
    [?_assertEqual({ok, [2, 3]},
                   optic:get([optic_lists:tail([strict])], [1, 2, 3])),
     ?_assertEqual({error, undefined},
                   optic:get([optic_lists:tail([strict])], atom))].

tail_put_test_() ->
    [?_assertEqual({ok, [1, 4, 4]},
                   optic:put([optic_lists:tail([strict])], [1, 2, 3], 4)),
     ?_assertEqual({error, undefined},
                   optic:put([optic_lists:tail([strict])], atom, 4))].

tail_create_test() ->
    ?assertEqual({ok, [undefined]},
                 optic:put([optic_lists:tail([strict,
                                              {create, undefined}])],
                           atom,
                           4)).

nth_get_test_() ->
    [?_assertEqual({ok, [1]},
                   optic:get([optic_lists:nth(1, [strict])], [1, 2, 3])),
     ?_assertEqual({error, undefined},
                   optic:get([optic_lists:nth(1, [strict])], atom))].

nth_put_test_() ->
    [?_assertEqual({ok, [4, 2, 3]},
                   optic:put([optic_lists:nth(1, [strict])], [1, 2, 3], 4)),
     ?_assertEqual({error, undefined},
                   optic:put([optic_lists:nth(1, [strict])], atom, 4))].

nth_create_test_() ->
    [?_assertEqual({ok, [undefined, 4]},
                   optic:put([optic_lists:nth(2, [strict,
                                                  {create, undefined}])],
                             atom,
                             4)),
     ?_assertEqual({ok, [1, undefined, 4]},
                   optic:put([optic_lists:nth(3, [strict,
                                                  {create, undefined}])],
                             [1],
                             4))].
