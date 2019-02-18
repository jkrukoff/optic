%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for src/optic_path.erl
%%% @end
%%%-------------------------------------------------------------------
-module(test_optic_path).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

new_test_() ->
    [?_assertEqual({ok, [value]},
                   optic:get(optic_path:new(["key"]), #{"key"=>value})),
     ?_assertEqual({ok, [value]},
                   optic:get(optic_path:new(["key"]), [{"key", value}])),
     ?_assertEqual({ok, [value]},
                   optic:get(optic_path:new([<<"key">>]),
                             #{<<"key">>=>value})),
     ?_assertEqual({ok, [value]},
                   optic:get(optic_path:new([1]), [value])),
     ?_assertEqual({ok, [1, 2, 3]},
                   optic:get(optic_path:new(['*']), [1, 2, 3])),
     ?_assertEqual({ok, [value]},
                   optic:get(optic_path:new(["key", 1, "nested"]),
                             #{"key"=>[#{"nested"=>value}]}))].
