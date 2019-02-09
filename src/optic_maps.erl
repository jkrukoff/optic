%%%-------------------------------------------------------------------
%%% @doc
%%% A set of optics specific to maps.
%%% @end
%%%-------------------------------------------------------------------
-module(optic_maps).

%% API
-export([all/0,
         all/1,
         keys/0,
         keys/1,
         values/0,
         values/1,
         associations/0,
         associations/1,
         key/1,
         key/2,
         association/1,
         association/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec all() -> optic:optic().
all() ->
    values().

-spec all(optic:extend_options()) -> optic:optic().
all(Options) ->
    values(Options).

-spec keys() -> optic:optic().
keys() ->
    keys(#{}).

-spec keys(optic:extend_options()) -> optic:optic().
keys(Options) ->
    Fold = fun (Fun, Acc, Map) when is_map(Map) ->
        {ok, maps:fold(fun (Key, _Value, InnerAcc) ->
                           Fun(Key, InnerAcc)
                       end,
                       Acc,
                       Map)};
    (_Fun, _Acc, _Data) ->
        {error, undefined}
    end,
    MapFold = fun (Fun, Acc, Map) when is_map(Map) ->
        {ok, maps:fold(fun (Key, Value, {InnerMap, InnerAcc}) ->
                           {NewKey, NewAcc} = Fun(Key, InnerAcc),
                           {InnerMap#{NewKey=>Value}, NewAcc}
                       end,
                       {#{}, Acc},
                       Map)};
    (_Fun, _Acc, _Data) ->
        {error, undefined}
    end,
    New = fun (_Data, _Template) ->
        #{}
    end,
    Optic = optic:new(MapFold, Fold),
    optic:'%extend'(Optic, Options, New).

-spec values() -> optic:optic().
values() ->
    values(#{}).

-spec values(optic:extend_options()) -> optic:optic().
values(Options) ->
    Fold = fun (Fun, Acc, Map) when is_map(Map) ->
        {ok, maps:fold(fun (_Key, Value, InnerAcc) ->
                           Fun(Value, InnerAcc)
                       end,
                       Acc,
                       Map)};
    (_Fun, _Acc, _Data) ->
        {error, undefined}
    end,
    MapFold = fun (Fun, Acc, Map) when is_map(Map) ->
        {ok, maps:fold(fun (Key, Value, {InnerMap, InnerAcc}) ->
                           {NewValue, NewAcc} = Fun(Value, InnerAcc),
                           {InnerMap#{Key=>NewValue}, NewAcc}
                       end,
                       {#{}, Acc},
                       Map)};
    (_Fun, _Acc, _Data) ->
        {error, undefined}
    end,
    New = fun (_Data, _Template) ->
        #{}
    end,
    Optic = optic:new(MapFold, Fold),
    optic:'%extend'(Optic, Options, New).

-spec associations() -> optic:optic().
associations() ->
    associations(#{}).

-spec associations(optic:extend_options()) -> optic:optic().
associations(Options) ->
    Fold = fun (Fun, Acc, Map) when is_map(Map) ->
        {ok, maps:fold(fun (Key, Value, InnerAcc) ->
                           Fun({Key, Value}, InnerAcc)
                       end,
                       Acc,
                       Map)};
    (_Fun, _Acc, _Data) ->
        {error, undefined}
    end,
    MapFold = fun (Fun, Acc, Map) when is_map(Map) ->
        {ok, maps:fold(fun (Key, Value, {InnerMap, InnerAcc}) ->
                           {{NewKey, NewValue}, NewAcc} = Fun({Key, Value}, InnerAcc),
                           {InnerMap#{NewKey=>NewValue}, NewAcc}
                       end,
                       {#{}, Acc},
                       Map)};
    (_Fun, _Acc, _Data) ->
        {error, undefined}
    end,
    New = fun (_Data, _Template) ->
        #{}
    end,
    Optic = optic:new(MapFold, Fold),
    optic:'%extend'(Optic, Options, New).

-spec key(term()) -> optic:optic().
key(Key) ->
    key(Key, #{}).

-spec key(term(), optic:extend_options()) -> optic:optic().
key(Key, Options) ->
    Fold = fun (Fun, Acc, #{Key:=Value}) ->
        {ok, Fun(Value, Acc)};
    (_Fun, _Acc, _Data) ->
        {error, undefined}
    end,
    MapFold = fun (Fun, Acc, #{Key:=Value} = Map) when is_map(Map) ->
        {NewValue, NewAcc} = Fun(Value, Acc),
        {ok, {Map#{Key:=NewValue}, NewAcc}};
    (_Fun, _Acc, _Data) ->
        {error, undefined}
    end,
    New = fun (Map, Template) when is_map(Map) ->
        Map#{Key=>Template};
    (_Data, Template) ->
        #{Key=>Template}
    end,
    Optic = optic:new(MapFold, Fold),
    optic:'%extend'(Optic, Options, New).

-spec association(term()) -> optic:optic().
association(Key) ->
    association(Key, #{}).

-spec association(term(), optic:extend_options()) -> optic:optic().
association(Key, Options) ->
    Fold = fun (Fun, Acc, #{Key:=Value}) ->
        {ok, Fun({Key, Value}, Acc)};
    (_Fun, _Acc, _Data) ->
        {error, undefined}
    end,
    MapFold = fun (Fun, Acc, #{Key:=Value} = Map) when is_map(Map) ->
        {{NewKey, NewValue}, NewAcc} = Fun({Key, Value}, Acc),
        {ok, {(maps:remove(Key, Map))#{NewKey=>NewValue}, NewAcc}};
    (_Fun, _Acc, _Data) ->
        {error, undefined}
    end,
    New = fun (Map, Template) when is_map(Map) ->
        Map#{Key=>Template};
    (_Data, Template) ->
        #{Key=>Template}
    end,
    Optic = optic:new(MapFold, Fold),
    optic:'%extend'(Optic, Options, New).
