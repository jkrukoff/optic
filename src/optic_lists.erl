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

%% @see all/1
-spec all() -> optic:optic().
all() ->
    all(#{}).

%% @doc
%% Focus on all elements of a list.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_lists:all()], [1,2,3]).
%% {ok,[1,2,3]}
%% '''
%% @end
%% @param Options Common optic options.
%% @returns An opaque optic record.
-spec all(Options) -> optic:optic() when
      Options :: optic:variations().
all(Options) ->
    Fold =
    fun (Fun, Acc, List) when is_list(List) ->
            {ok, lists:foldl(Fun, Acc, List)};
        (_, _, _) ->
            {error, undefined}
    end,
    MapFold =
    fun (Fun, Acc, List) when is_list(List) ->
            {ok, lists:mapfoldl(Fun, Acc, List)};
        (_, _, _) ->
            {error, undefined}
    end,
    New =
    fun (_Data, _Template) ->
            []
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%% @see head/1
-spec head() -> optic:optic().
head() ->
    head(#{}).

%% @doc
%% Focus on the head of a list. The list must have at least one
%% element to have a head.
%% 
%% Example:
%%
%% ```
%% > optic:get([optic_lists:head()], [1,2,3]).
%% {ok,[1]}
%% '''
%% @end
%% @param Options Common optic options.
%% @returns An opaque optic record.
-spec head(Options) -> optic:optic() when
      Options :: optic:variations().
head(Options) ->
    Fold =
    fun (Fun, Acc, [Head | _]) ->
            {ok, Fun(Head, Acc)};
        (_, _, _) ->
            {error, undefined}
    end,
    MapFold =
    fun (Fun, Acc, [Head | Tail]) ->
            {NewHead, NewAcc} = Fun(Head, Acc),
            {ok, {[NewHead | Tail], NewAcc}};
        (_, _, _) ->
            {error, undefined}
    end,
    New =
    fun (_Data, Template) ->
            [Template]
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%% @see tail/1
-spec tail() -> optic:optic().
tail() ->
    tail(#{}).

%% @doc
%% Focus on the tail of a list. A list must have at least one element
%% to have a tail.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_lists:tail()], [1,2,3]).
%% {ok,[2,3]}
%% '''
%% @end
%% @param Options Common optic options.
%% @returns An opaque optic record.
-spec tail(Options) -> optic:optic() when
      Options :: optic:variations().
tail(Options) ->
    Fold =
    fun (Fun, Acc, [_ | Tail]) ->
            {ok, lists:foldl(Fun, Acc, Tail)};
        (_, _, _) ->
            {error, undefined}
    end,
    MapFold =
    fun (Fun, Acc, [Head | Tail]) ->
            {NewTail, NewAcc} = lists:mapfoldl(Fun, Acc, Tail),
            {ok, {[Head | NewTail], NewAcc}};
        (_, _, _) ->
            {error, undefined}
    end,
    New =
    fun (_Data, Template) ->
            [Template]
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%% @see nth/2
-spec nth(N) -> optic:optic() when
      N :: pos_integer().
nth(N) ->
    nth(N, #{}).

%% @doc
%% Focus on the nth element of a list. As with `lists:nth/2', indexing
%% begins at 1.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_lists:nth(1)], [1,2,3]).
%% {ok,[1]}
%% '''
%% @end
%% @param N The index of the list element to focus on.
%% @param Options Common optic options.
%% @returns An opaque optic record.
-spec nth(N, Options) -> optic:optic() when
      N :: pos_integer(),
      Options :: optic:variations().
nth(N, Options) ->
    Fold =
    fun (Fun, Acc, List) when N =< length(List) ->
            Nth = lists:nth(N, List),
            {ok, Fun(Nth, Acc)};
        (_, _, _) ->
            {error, undefined}
    end,
    MapFold =
    fun (Fun, Acc, List) when N =< length(List) ->
            {Before, [Head | Tail]} = lists:split(N - 1, List),
            {NewHead, NewAcc} = Fun(Head, Acc),
            {ok, {Before ++ [NewHead] ++ Tail, NewAcc}};
        (_, _, _) ->
            {error, undefined}
    end,
    New =
    fun (Data, Template) when is_list(Data) ->
            Data ++ lists:duplicate(N - length(Data), Template);
        (_Data, Template) ->
            lists:duplicate(N, Template)
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).
