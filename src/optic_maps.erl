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

%% @see values/1
-spec all() -> optic:optic().
all() ->
    values().

%% @see values/1
-spec all(Options) -> optic:optic() when
      Options :: optic:variations().
all(Options) ->
    values(Options).

%% @see keys/1
-spec keys() -> optic:optic().
keys() ->
    keys(#{}).

%% @doc
%% Focus on all keys of a map.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_maps:keys()], #{first => 1, second => 2}).
%% {ok,[first,second]}
%% '''
%% @end
%% @param Options Common optic options.
%% @returns An opaque optic record.
-spec keys(Options) -> optic:optic() when
      Options :: optic:variations().
keys(Options) ->
    Fold =
    fun (Fun, Acc, Map) when is_map(Map) ->
            {ok, maps:fold(fun (Key, _Value, InnerAcc) ->
                                   Fun(Key, InnerAcc)
                           end,
                           Acc,
                           Map)};
        (_Fun, _Acc, _Data) ->
            {error, undefined}
    end,
    MapFold =
    fun (Fun, Acc, Map) when is_map(Map) ->
            {ok, maps:fold(fun (Key, Value, {InnerMap, InnerAcc}) ->
                                   {NewKey, NewAcc} = Fun(Key, InnerAcc),
                                   {InnerMap#{NewKey=>Value}, NewAcc}
                           end,
                           {#{}, Acc},
                           Map)};
        (_Fun, _Acc, _Data) ->
            {error, undefined}
    end,
    New =
    fun (_Data, _Template) ->
            #{}
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%% @see values/1
-spec values() -> optic:optic().
values() ->
    values(#{}).

%% @doc
%% Focus on all values of a map.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_maps:values()], #{first => 1, second => 2}).
%% {ok,[1,2]}
%% '''
%% @end
%% @param Options Common optic options.
%% @returns An opaque optic record.
-spec values(Options) -> optic:optic() when
      Options :: optic:variations().
values(Options) ->
    Fold =
    fun (Fun, Acc, Map) when is_map(Map) ->
            {ok, maps:fold(fun (_Key, Value, InnerAcc) ->
                                   Fun(Value, InnerAcc)
                           end,
                           Acc,
                           Map)};
        (_Fun, _Acc, _Data) ->
            {error, undefined}
    end,
    MapFold =
    fun (Fun, Acc, Map) when is_map(Map) ->
            {ok, maps:fold(fun (Key, Value, {InnerMap, InnerAcc}) ->
                                   {NewValue, NewAcc} = Fun(Value, InnerAcc),
                                   {InnerMap#{Key=>NewValue}, NewAcc}
                           end,
                           {#{}, Acc},
                           Map)};
        (_Fun, _Acc, _Data) ->
            {error, undefined}
    end,
    New =
    fun (_Data, _Template) ->
            #{}
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%% @see associations/1
-spec associations() -> optic:optic().
associations() ->
    associations(#{}).

%% @doc
%% Focus on all associations of a map. An association is a tuple of
%% the key and value for each entry.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_maps:associations()], #{first => 1, second => 2}).
%% {ok,[{first,1},{second,2}]}
%% '''
%% @end
%% @param Options Common optic options.
%% @returns An opaque optic record.
-spec associations(Options) -> optic:optic() when
      Options :: optic:variations().
associations(Options) ->
    Fold =
    fun (Fun, Acc, Map) when is_map(Map) ->
            {ok, maps:fold(fun (Key, Value, InnerAcc) ->
                                   Fun({Key, Value}, InnerAcc)
                           end,
                           Acc,
                           Map)};
        (_Fun, _Acc, _Data) ->
            {error, undefined}
    end,
    MapFold =
    fun (Fun, Acc, Map) when is_map(Map) ->
            {ok, maps:fold(fun (Key, Value, {InnerMap, InnerAcc}) ->
                                   {{NewKey, NewValue}, NewAcc} = Fun({Key, Value}, InnerAcc),
                                   {InnerMap#{NewKey=>NewValue}, NewAcc}
                           end,
                           {#{}, Acc},
                           Map)};
        (_Fun, _Acc, _Data) ->
            {error, undefined}
    end,
    New =
    fun (_Data, _Template) ->
            #{}
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%% @see key/2
-spec key(Key) -> optic:optic() when
    Key :: term().
key(Key) ->
    key(Key, #{}).

%% @doc
%% Focus on the value of a map key.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_maps:key(first)], #{first => 1, second => 2}).
%% {ok,[1]}
%% '''
%% @end
%% @param Key The key to focus on.
%% @param Options Common optic options.
%% @returns An opaque optic record.
-spec key(Key, Options) -> optic:optic() when
      Key :: term(),
      Options :: optic:variations().
key(Key, Options) ->
    Fold =
    fun (Fun, Acc, #{Key:=Value}) ->
            {ok, Fun(Value, Acc)};
        (_Fun, _Acc, _Data) ->
            {error, undefined}
    end,
    MapFold =
    fun (Fun, Acc, #{Key:=Value} = Map) when is_map(Map) ->
            {NewValue, NewAcc} = Fun(Value, Acc),
            {ok, {Map#{Key:=NewValue}, NewAcc}};
        (_Fun, _Acc, _Data) ->
            {error, undefined}
    end,
    New =
    fun (Map, Template) when is_map(Map) ->
            Map#{Key=>Template};
        (_Data, Template) ->
            #{Key=>Template}
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%% @see association/2
-spec association(Key) -> optic:optic() when
      Key :: term().
association(Key) ->
    association(Key, #{}).

%% @doc
%% Focus on the association for a map key. An association is the tuple
%% of a map key and value. If the key is modified, the optic is no
%% longer well behaved.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_maps:association(first)], #{first => 1, second => 2}).
%% {ok,[{first,1}]}
%% '''
%% @end
%% @param Key The key to focus on.
%% @param Options Common optic options.
%% @returns An opaque optic record.
-spec association(Key, Options) -> optic:optic() when
      Key :: term(),
      Options :: optic:variations().
association(Key, Options) ->
    Fold =
    fun (Fun, Acc, #{Key:=Value}) ->
            {ok, Fun({Key, Value}, Acc)};
        (_Fun, _Acc, _Data) ->
            {error, undefined}
    end,
    MapFold =
    fun (Fun, Acc, #{Key:=Value} = Map) when is_map(Map) ->
            {{NewKey, NewValue}, NewAcc} = Fun({Key, Value}, Acc),
            {ok, {(maps:remove(Key, Map))#{NewKey=>NewValue}, NewAcc}};
        (_Fun, _Acc, _Data) ->
            {error, undefined}
    end,
    New =
    fun (Map, Template) when is_map(Map) ->
            Map#{Key=>Template};
        (_Data, Template) ->
            #{Key=>Template}
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).
