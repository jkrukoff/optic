%%%-------------------------------------------------------------------
%%% @doc
%%% A set of optics specific to lists.
%%% @end
%%%-------------------------------------------------------------------
-module(optic_lists).

%% API
-export([all/0,
         all/1,
         head/0,
         head/1,
         tail/0,
         tail/1,
         nth/1,
         nth/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec all() -> optic:optic().
all() ->
    all(#{}).

-spec all(optic:extend_options()) -> optic:optic().
all(Options) ->
    Fold = fun (Fun, Acc, List) when is_list(List) ->
        {ok, lists:foldl(Fun, Acc, List)};
    (_, _, _) ->
        {error, undefined}
    end,
    MapFold = fun (Fun, Acc, List) when is_list(List) ->
        {ok, lists:mapfoldl(Fun, Acc, List)};
    (_, _, _) ->
        {error, undefined}
    end,
    New = fun (_Data, _Template) ->
        []
    end,
    Optic = optic:new(Fold, MapFold),
    optic:'%extend'(Optic, Options, New).

-spec head() -> optic:optic().
head() ->
    head(#{}).

-spec head(optic:extend_options()) -> optic:optic().
head(Options) ->
    Fold = fun (Fun, Acc, [Head | _]) ->
        {ok, Fun(Head, Acc)};
    (_, _, _) ->
        {error, undefined}
    end,
    MapFold = fun (Fun, Acc, [Head | Tail]) ->
        {NewHead, NewAcc} = Fun(Head, Acc),
        {ok, {[NewHead | Tail], NewAcc}};
    (_, _, _) ->
        {error, undefined}
    end,
    New = fun (_Data, Template) ->
        [Template]
    end,
    Optic = optic:new(Fold, MapFold),
    optic:'%extend'(Optic, Options, New).

-spec tail() -> optic:optic().
tail() ->
    tail(#{}).

-spec tail(optic:extend_options()) -> optic:optic().
tail(Options) ->
    Fold = fun (Fun, Acc, [_ | Tail]) ->
        {ok, lists:foldl(Fun, Acc, Tail)};
    (_, _, _) ->
        {error, undefined}
    end,
    MapFold = fun (Fun, Acc, [Head | Tail]) ->
        {NewTail, NewAcc} = lists:mapfoldl(Fun, Acc, Tail),
        {ok, {[Head | NewTail], NewAcc}};
    (_, _, _) ->
        {error, undefined}
    end,
    New = fun (_Data, Template) ->
        [Template]
    end,
    Optic = optic:new(Fold, MapFold),
    optic:'%extend'(Optic, Options, New).

-spec nth(pos_integer()) -> optic:optic().
nth(N) ->
    nth(N, #{}).

-spec nth(pos_integer(), optic:extend_options()) -> optic:optic().
nth(N, Options) ->
    Fold = fun (Fun, Acc, List) when N =< length(List) ->
        Nth = lists:nth(N, List),
        {ok, Fun(Nth, Acc)};
    (_, _, _) ->
        {error, undefined}
    end,
    MapFold = fun (Fun, Acc, List) when N =< length(List) ->
        {Before, [Head | Tail]} = lists:split(N - 1, List),
        {NewHead, NewAcc} = Fun(Head, Acc),
        {ok, {Before ++ [NewHead] ++ Tail, NewAcc}};
    (_, _, _) ->
        {error, undefined}
    end,
    New = fun (Data, Template) when is_list(Data) ->
        Data ++ lists:duplicate(N - length(Data), Template);
    (_Data, Template) ->
        lists:duplicate(N, Template)
    end,
    Optic = optic:new(Fold, MapFold),
    optic:'%extend'(Optic, Options, New).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
