%%%-------------------------------------------------------------------
%%% @doc
%%% A set of optics specific to tuples.
%%% @end
%%%-------------------------------------------------------------------
-module(optic_tuples).

%% API
-export([all/0,
         all/1,
         element/1,
         element/2,
         field/3,
         field/4]).

%%%===================================================================
%%% API
%%%===================================================================

%% @see all/1
-spec all() -> optic:optic().
all() ->
    all(#{}).

%% @doc
%% Focus on all elements of a tuple.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_tuples:all()], {1,2,3}).
%% {ok,[1,2,3]}
%% '''
%% @end
%% @param Options Common optic options.
%% @returns An opaque optic record.
-spec all(Options) -> optic:optic() when
      Options :: optic:variations().
all(Options) ->
    Fold =
    fun (Fun, Acc, Tuple) when is_tuple(Tuple) ->
            {ok, lists:foldl(Fun, Acc, tuple_to_list(Tuple))};
        (_, _, _) ->
            {error, undefined}
    end,
    MapFold =
    fun (Fun, Acc, Tuple) when is_tuple(Tuple) ->
            {NewList, NewAcc} = lists:mapfoldl(Fun, Acc, tuple_to_list(Tuple)),
            {ok, {list_to_tuple(NewList), NewAcc}};
        (_, _, _) ->
            {error, undefined}
    end,
    New =
    fun (_Data, _Template) ->
            {}
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%% @see element/2
-spec element(N) -> optic:optic() when
      N :: pos_integer().
element(N) ->
    optic_tuples:element(N, #{}).

%% @doc
%% Focus on the nth element of a tuple. As with `erlang:element/2',
%% indexing begins at 1.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_tuples:element(1)], {1,2,3}).
%% {ok,[1]}
%% '''
%% @end
%% @param N The index of the tuple element to focus on.
%% @param Options Common optic options.
%% @returns An opaque optic record.
-spec element(N, Options) -> optic:optic() when
      N :: pos_integer(),
      Options :: optic:variations().
element(N, Options) ->
    Fold =
    fun (Fun, Acc, Tuple) when N =< tuple_size(Tuple) ->
            Nth = erlang:element(N, Tuple),
            {ok, Fun(Nth, Acc)};
        (_, _, _) ->
            {error, undefined}
    end,
    MapFold =
    fun (Fun, Acc, Tuple) when N =< tuple_size(Tuple) ->
            Nth = erlang:element(N, Tuple),
            {NewNth, NewAcc} = Fun(Nth, Acc),
            {ok, {erlang:setelement(N, Tuple, NewNth), NewAcc}};
        (_, _, _) ->
            {error, undefined}
    end,
    New =
    fun (Tuple, Template) when is_tuple(Tuple) ->
            list_to_tuple(tuple_to_list(Tuple) ++
                          lists:duplicate(N - tuple_size(Tuple), Template));
        (_Data, Template) ->
            list_to_tuple(lists:duplicate(N, Template))
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%% @see field/4
-spec field(Tag, Size, N) -> optic:optic() when
      Tag :: atom(),
      Size :: pos_integer(),
      N :: pos_integer().
field(Tag, Size, N) ->
    field(Tag, Size, N, #{}).

%% @doc
%% Focus on a record field. As records are a compiler construct, this
%% depends on the `?OPTIC_FIELD' macro in `include/optic_tuples.hrl'
%% to construct the required arguments from the record definition.
%%
%% Given the record definition:
%%
%% ```
%% -include_lib("optic/include/optic_tuples.hrl").
%% -record(example, {first}).
%% '''
%%
%% Example:
%%
%% ```
%% > optic:get([optic_tuples:field(?OPTIC_FIELD(example, first))],
%%             #example{first=1}).
%% {ok,[1]}
%% '''
%% @end
%% @param Tag The expected record tag.
%% @param Size The expected record size.
%% @param N The index of the field in the record tuple.
%% @param Options Common optic options.
%% @returns An opaque optic record.
-spec field(Tag, Size, N, Options) -> optic:optic() when
      Tag :: atom(),
      Size :: pos_integer(),
      N :: pos_integer(),
      Options :: optic:variations().
field(Tag, Size, N, Options) ->
    Fold =
    fun (Fun, Acc, Tuple) when erlang:element(1, Tuple) == Tag,
                               Size == tuple_size(Tuple),
                               N > 1,
                               N =< tuple_size(Tuple) ->
            Nth = erlang:element(N, Tuple),
            {ok, Fun(Nth, Acc)};
        (_, _, _) ->
            {error, undefined}
    end,
    MapFold =
    fun (Fun, Acc, Tuple) when erlang:element(1, Tuple) == Tag,
                               Size == tuple_size(Tuple),
                               N > 1,
                               N =< tuple_size(Tuple) ->
            Nth = erlang:element(N, Tuple),
            {NewNth, NewAcc} = Fun(Nth, Acc),
            {ok, {erlang:setelement(N, Tuple, NewNth), NewAcc}};
        (_, _, _) ->
            {error, undefined}
    end,
    New =
    fun (_Data, Template) ->
            list_to_tuple([Tag] ++ lists:duplicate(Size - 1, Template))
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).
