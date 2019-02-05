%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(optic_lists).

%% API
-export([all/0,
         all_opt/0,
         head/0,
         head_opt/0,
         head_opt/1,
         tail/0,
         tail_opt/0,
         tail_opt/1,
         nth/1,
         nth_opt/1,
         nth_opt/2]).

%%%===================================================================
%%% API
%%%===================================================================

all() ->
    Get = fun (List) when is_list(List) ->
        {ok, List};
    (_) ->
        {error, undefined}
    end,
    Put = fun (List, Value) when length(List) == length(Value) ->
        {ok, Value};
    (_, _) ->
        {error, undefined}
    end,
    Del = fun (List) when is_list(List) ->
        {ok, []};
    (_) ->
        {error, undefined}
    end,
    optic:new(Get, Put, undefined, Del).

all_opt() ->
    New = fun () -> [] end,
    optic:from(all(), #{new=>New}).

head() ->
    Get = fun ([Head | _]) ->
        {ok, [Head]};
    (_) ->
        {error, undefined}
    end,
    Put = fun ([_ | Tail], [Head]) ->
        {ok, [Head | Tail]};
    (_, _) ->
        {error, undefined}
    end,
    Del = fun ([_ | Tail]) ->
        {ok, Tail};
    (_) ->
        {error, undefined}
    end,
    optic:new(Get, Put, undefined, Del).

head_opt() ->
    head_opt(undefined).

head_opt(NewValue) ->
    New = fun () -> [NewValue] end,
    optic:from(head(), #{new=>New}).

tail() ->
    Get = fun ([_ | Tail]) ->
        {ok, [Tail]};
    (_) ->
        {error, undefined}
    end,
    Put = fun ([Head | _], [Tail]) when is_list(Tail) ->
        {ok, [Head | Tail]};
    (_, _) ->
        {error, undefined}
    end,
    Del = fun ([Head | _]) ->
        {ok, [Head]};
    (_) ->
        {error, undefined}
    end,
    optic:new(Get, Put, undefined, Del).

tail_opt() ->
    tail_opt(undefined).

tail_opt(NewValue) ->
    New = fun () -> [NewValue] end,
    optic:from(tail(), #{new=>New}).

nth(N) ->
    Get = fun (List) when N =< length(List) ->
        {ok, [lists:nth(N, List)]};
    (_) ->
        {error, undefined}
    end,
    Put = fun (List, [Value]) when N =< length(List) ->
        {Before, [_ | Tail]} = lists:split(N, List),
        {ok, Before ++ [Value] ++ Tail};
    (_, _) ->
        {error, undefined}
    end,
    Del = fun (List) when N =< length(List) ->
        {Before, [_ | Tail]} = lists:split(N, List),
        {ok, Before ++ Tail};
    (_) ->
        {error, undefined}
    end,
    optic:new(Get, Put, undefined, Del).

nth_opt(N) ->
    nth_opt(N, undefined).

nth_opt(N, NewValue) ->
    New = fun () ->
        lists:duplicate(N, NewValue)
    end,
    optic:from(nth(N), #{new=>New}).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
