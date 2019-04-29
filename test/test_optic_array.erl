%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for src/optic_array.erl
%%% @end
%%%-------------------------------------------------------------------
-module(test_optic_array).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

all_get_test_() ->
    [?_assertEqual({ok, [1, 2, 3]},
                   optic:get([optic_array:all([strict])],
                             array:from_list([1, 2, 3]))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_array:all([strict])],
                             atom))].

all_put_test_() ->
    [?_assertEqual({ok, array:from_list([4, 4, 4])},
                   optic:put([optic_array:all([strict])],
                             4,
                             array:from_list([1, 2, 3]))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_array:all([strict])],
                             4,
                             atom))].

all_create_test() ->
    ?assertEqual({ok, array:new([{default, test}])},
                 optic:put([optic_array:all([strict, {create, test}])],
                           4,
                           atom)).

nth_get_test_() ->
    [?_assertEqual({ok, [1]},
                   optic:get([optic_array:nth(1, [strict])],
                             array:from_list([1, 2, 3]))),
     ?_assertEqual({error, undefined},
                   optic:get([optic_array:nth(1, [strict])],
                             atom))].

nth_put_test_() ->
    [?_assertEqual({ok, array:from_list([4, 2, 3])},
                   optic:put([optic_array:nth(1, [strict])],
                             4,
                             array:from_list([1, 2, 3]))),
     ?_assertEqual({error, undefined},
                   optic:put([optic_array:nth(1, [strict])],
                             4,
                             atom))].

nth_create_test_() ->
    [?_assertEqual({ok, [test, 4]},
                   maybe_to_list(
                     optic:put([optic_array:nth(2, [strict, {create, test}])],
                               4,
                               atom))),
     ?_assertEqual({ok, [1, undefined, 4]},
                   maybe_to_list(
                     optic:put([optic_array:nth(3, [strict, create])],
                               4,
                               array:from_list([1]))))].

%%%===================================================================
%%% Internal Functions
%%%===================================================================

maybe_to_list({ok, Result}) ->
    {ok, array:to_list(Result)}.
