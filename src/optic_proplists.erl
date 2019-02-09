%%%-------------------------------------------------------------------
%%% @doc
%%% A set of optics specific to proplists.
%%% @end
%%%-------------------------------------------------------------------
-module(optic_proplists).

%% API
-export([all/0,
         all/1,
         keys/0,
         keys/1,
         values/0,
         values/1,
         properties/0,
         properties/1,
         key/1,
         key/2,
         property/1,
         property/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec all() -> optic:optic().
all() ->
    values(#{}).

-spec all(optic:extend_options()) -> optic:optic().
all(Options) ->
    values(Options).

-spec keys() -> optic:optic().
keys() ->
    keys(#{}).

-spec keys(optic:extend_options()) -> optic:optic().
keys(Options) ->
    Fold = fun (Fun, Acc, List) when is_list(List) ->
        {ok, lists:foldl(fun ({Key, _Value}, InnerAcc) ->
                             Fun(Key, InnerAcc)
                         end,
                         Acc,
                         proplists:unfold(List))};
    (_Fun, _Acc, _Data) ->
        {error, undefined}
    end,
    MapFold = fun (Fun, Acc, List) when is_list(List) ->
        {NewList, NewAcc} = lists:foldl(fun ({Key, Value}, {InnerList, InnerAcc}) ->
                                           {NewKey, NewAcc} = Fun(Key, InnerAcc),
                                           {[{NewKey, Value} | InnerList], NewAcc}
                                       end,
                                       {[], Acc},
                                       proplists:unfold(List)),
        {ok, {lists:reverse(NewList), NewAcc}};
    (_Fun, _Acc, _Data) ->
        {error, undefined}
    end,
    New = fun (_Data, _Template) ->
        []
    end,
    Optic = optic:new(Fold, MapFold),
    optic:'%extend'(Optic, Options, New).

-spec values() -> optic:optic().
values() ->
    values(#{}).

-spec values(optic:extend_options()) -> optic:optic().
values(Options) ->
    Fold = fun (Fun, Acc, List) when is_list(List) ->
        {ok, lists:foldl(fun ({_Key, Value}, InnerAcc) ->
                             Fun(Value, InnerAcc)
                         end,
                         Acc,
                         proplists:unfold(List))};
    (_Fun, _Acc, _Data) ->
        {error, undefined}
    end,
    MapFold = fun (Fun, Acc, List) when is_list(List) ->
        {NewList, NewAcc} = lists:foldl(fun ({Key, Value}, {InnerList, InnerAcc}) ->
                                            {NewValue, NewAcc} = Fun(Value, InnerAcc),
                                            {[{Key, NewValue} | InnerList], NewAcc}
                                        end,
                                        {[], Acc},
                                        proplists:unfold(List)),
        {ok, {lists:reverse(NewList), NewAcc}};
    (_Fun, _Acc, _Data) ->
        {error, undefined}
    end,
    New = fun (_Data, _Template) ->
        []
    end,
    Optic = optic:new(Fold, MapFold),
    optic:'%extend'(Optic, Options, New).

-spec properties() -> optic:optic().
properties() ->
    properties(#{}).

-spec properties(optic:extend_options()) -> optic:optic().
properties(Options) ->
    Fold = fun (Fun, Acc, List) when is_list(List) ->
        {ok, lists:foldl(Fun, Acc, proplists:unfold(List))};
    (_Fun, _Acc, _Data) ->
        {error, undefined}
    end,
    MapFold = fun (Fun, Acc, List) when is_list(List) ->
        {NewList, NewAcc} = lists:foldl(fun (Tuple, {InnerList, InnerAcc}) ->
                                            {NewTuple, NewAcc} = Fun(Tuple, InnerAcc),
                                            {[NewTuple | InnerList], NewAcc}
                                        end,
                                        {[], Acc},
                                        proplists:unfold(List)),
        {ok, {lists:reverse(NewList), NewAcc}};
    (_Fun, _Acc, _Data) ->
        {error, undefined}
    end,
    New = fun (_Data, _Template) ->
        []
    end,
    Optic = optic:new(Fold, MapFold),
    optic:'%extend'(Optic, Options, New).

-spec key(term()) -> optic:optic().
key(Key) ->
    key(Key, #{}).

-spec key(term(), optic:extend_options()) -> optic:optic().
key(Key, Options) ->
    Fold = fun (Fun, Acc, List) when is_list(List) ->
        case proplists:get_all_values(Key, List) of
            [] ->
                {error, undefined};
            Values ->
                {ok, lists:foldl(fun (Value, InnerAcc) ->
                                     Fun(Value, InnerAcc)
                                 end,
                                 Acc,
                                 Values)}
        end;
    (_Fun, _Acc, _Data) ->
        {error, undefined}
    end,
    MapFold = fun (Fun, Acc, List) when is_list(List) ->
        case proplists:get_all_values(Key, List) of
            [] ->
                {error, undefined};
            _ ->
                {ok, lists:mapfoldl(fun (Elem, InnerAcc) ->
                                        case proplists:is_defined(Key, [Elem]) of
                                            true ->
                                                Value = proplists:get_value(Key, [Elem]),
                                                {NewValue, NewAcc} = Fun(Value, InnerAcc),
                                                {{Key, NewValue}, NewAcc};
                                            false ->
                                                {Elem, InnerAcc}
                                        end
                                    end,
                                    Acc,
                                    List)}
        end;
    (_Fun, _Acc, _Data) ->
        {error, undefined}
    end,
    New = fun (List, Template) when is_list(List) ->
        [{Key, Template} | List];
    (_Data, Template) ->
        [{Key, Template}]
    end,
    Optic = optic:new(Fold, MapFold),
    optic:'%extend'(Optic, Options, New).

-spec property(term()) -> optic:optic().
property(Key) ->
    property(Key, #{}).

-spec property(term(), optic:extend_options()) -> optic:optic().
property(Key, Options) ->
    Fold = fun (Fun, Acc, List) when is_list(List) ->
        case proplists:get_all_values(Key, List) of
            [] ->
                {error, undefined};
            Values ->
                {ok, lists:foldl(fun (Value, InnerAcc) ->
                                    Fun({Key, Value}, InnerAcc)
                                 end,
                                 Acc,
                                 Values)}
        end;
    (_Fun, _Acc, _Data) ->
        {error, undefined}
    end,
    MapFold = fun (Fun, Acc, List) when is_list(List) ->
        case proplists:get_all_values(Key, List) of
            [] ->
                {error, undefined};
            _ ->
                {ok, lists:mapfoldl(fun (Elem, InnerAcc) ->
                                        case proplists:is_defined(Key, [Elem]) of
                                            true ->
                                                Value = proplists:get_value(Key, [Elem]),
                                                {{NewKey, NewValue}, NewAcc} = Fun({Key, Value}, InnerAcc),
                                                {{NewKey, NewValue}, NewAcc};
                                            false ->
                                                {Elem, InnerAcc}
                                        end
                                    end,
                                    Acc,
                                    List)}
        end;
    (_Fun, _Acc, _Data) ->
        {error, undefined}
    end,
    New = fun (List, Template) when is_list(List) ->
        [{Key, Template} | List];
    (_Data, Template) ->
        [{Key, Template}]
    end,
    Optic = optic:new(Fold, MapFold),
    optic:'%extend'(Optic, Options, New).
