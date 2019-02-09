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

-spec all() -> optic:optic().
all() ->
    all(#{}).

-spec all(optic:extend_options()) -> optic:optic().
all(Options) ->
    Fold = fun (Fun, Acc, Tuple) when is_tuple(Tuple) ->
        {ok, lists:foldl(Fun, Acc, tuple_to_list(Tuple))};
    (_, _, _) ->
        {error, undefined}
    end,
    MapFold = fun (Fun, Acc, Tuple) when is_tuple(Tuple) ->
        {NewList, NewAcc} = lists:mapfoldl(Fun, Acc, tuple_to_list(Tuple)),
        {ok, {list_to_tuple(NewList), NewAcc}};
    (_, _, _) ->
        {error, undefined}
    end,
    New = fun (_Data, _Template) ->
        {}
    end,
    Optic = optic:new(MapFold, Fold),
    optic:'%extend'(Optic, Options, New).

-spec element(pos_integer()) -> optic:optic().
element(N) ->
    optic_tuples:element(N, #{}).

-spec element(pos_integer(), optic:extend_options()) -> optic:optic().
element(N, Options) ->
    Fold = fun (Fun, Acc, Tuple) when N =< tuple_size(Tuple) ->
        Nth = erlang:element(N, Tuple),
        {ok, Fun(Nth, Acc)};
    (_, _, _) ->
        {error, undefined}
    end,
    MapFold = fun (Fun, Acc, Tuple) when N =< tuple_size(Tuple) ->
        Nth = erlang:element(N, Tuple),
        {NewNth, NewAcc} = Fun(Nth, Acc),
        {ok, {erlang:setelement(N, Tuple, NewNth), NewAcc}};
    (_, _, _) ->
        {error, undefined}
    end,
    New = fun (Tuple, Template) when is_tuple(Tuple) ->
        list_to_tuple(tuple_to_list(Tuple) ++
                      lists:duplicate(N - tuple_size(Tuple), Template));
    (_Data, Template) ->
        list_to_tuple(lists:duplicate(N, Template))
    end,
    Optic = optic:new(MapFold, Fold),
    optic:'%extend'(Optic, Options, New).

-spec field(atom(), pos_integer(), pos_integer()) -> optic:optic().
field(Tag, Size, N) ->
    field(Tag, Size, N, #{}).

-spec field(atom(), pos_integer(), pos_integer(), optic:extend_options()) -> optic:optic().
field(Tag, Size, N, Options) ->
    Fold = fun (Fun, Acc, Tuple) when erlang:element(1, Tuple) == Tag,
                                      Size == tuple_size(Tuple),
                                      N > 1,
                                      N =< tuple_size(Tuple) ->
        Nth = erlang:element(N, Tuple),
        {ok, Fun(Nth, Acc)};
    (_, _, _) ->
        {error, undefined}
    end,
    MapFold = fun (Fun, Acc, Tuple) when erlang:element(1, Tuple) == Tag,
                                         Size == tuple_size(Tuple),
                                         N > 1,
                                         N =< tuple_size(Tuple) ->
        Nth = erlang:element(N, Tuple),
        {NewNth, NewAcc} = Fun(Nth, Acc),
        {ok, {erlang:setelement(N, Tuple, NewNth), NewAcc}};
    (_, _, _) ->
        {error, undefined}
    end,
    New = fun (_Data, Template) ->
        list_to_tuple([Tag] ++ lists:duplicate(Size - 1, Template))
    end,
    Optic = optic:new(MapFold, Fold),
    optic:'%extend'(Optic, Options, New).
