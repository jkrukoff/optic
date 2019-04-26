%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for src/optic_sets.erl
%%% @end
%%%-------------------------------------------------------------------
-module(test_optic_sets).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

all_get_test_() ->
    [?_assertEqual({ok, [1, 2, 3]},
                   sort_get(
                     optic:get([optic_sets:all([strict])],
                               sets:from_list([1, 2, 3])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_sets:all([strict])], atom))].

all_map_test() ->
    ?assertEqual({ok, [2, 4, 6]},
                 sort_put(
                   optic:map([optic_sets:all([strict])],
                             sets:from_list([1, 2, 3]),
                             fun (Elem) -> Elem * 2 end))).

all_put_test_() ->
    [?_assertEqual({ok, [4]},
                   sort_put(
                     optic:put([optic_sets:all([strict])],
                               sets:from_list([1, 2, 3]),
                               4))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_sets:all([strict])], atom, 4))].

all_create_test() ->
    ?assertEqual({ok, sets:new()},
                 optic:put([optic_sets:all([strict, create])],
                           atom,
                           4)).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

sort_get({ok, Result}) ->
    {ok, lists:sort(Result)}.

sort_put({ok, Result}) ->
    {ok, lists:sort(sets:to_list(Result))}.
