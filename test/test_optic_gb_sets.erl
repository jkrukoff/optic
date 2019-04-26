%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for src/optic_gb_sets.erl
%%% @end
%%%-------------------------------------------------------------------
-module(test_optic_gb_sets).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

all_get_test_() ->
    [?_assertEqual({ok, [1, 2, 3]},
                   sort_get(
                     optic:get([optic_gb_sets:all([strict])],
                               gb_sets:from_list([1, 2, 3])))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_gb_sets:all([strict])], atom))].

all_map_test() ->
    ?assertEqual({ok, [2, 4, 6]},
                 sort_put(
                   optic:map([optic_gb_sets:all([strict])],
                             fun (Elem) -> Elem * 2 end,
                             gb_sets:from_list([1, 2, 3])))).

all_put_test_() ->
    [?_assertEqual({ok, [4]},
                   sort_put(
                     optic:put([optic_gb_sets:all([strict])],
                               4,
                               gb_sets:from_list([1, 2, 3])))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_gb_sets:all([strict])], 4, atom))].

all_create_test() ->
    ?assertEqual({ok, gb_sets:new()},
                 optic:put([optic_gb_sets:all([strict, create])],
                           4,
                           atom)).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

sort_get({ok, Result}) ->
    {ok, lists:sort(Result)}.

sort_put({ok, Result}) ->
    {ok, lists:sort(gb_sets:to_list(Result))}.
