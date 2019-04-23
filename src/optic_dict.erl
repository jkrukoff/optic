%%%-------------------------------------------------------------------
%%% @doc
%%% A set of optics specific to dicts.
%%% @end
%%%-------------------------------------------------------------------
-module(optic_dict).

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
%% Focus on all keys of a dict.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_dict:keys()],
%%             dict:from_list([{first, 1}, {second, 2}])).
%% {ok,[first,second]}
%% '''
%% @end
%% @param Options Common optic options.
%% @returns An opaque optic record.
-spec keys(Options) -> optic:optic() when
      Options :: optic:variations().
keys(Options) ->
    Fold =
    fun (Fun, Acc, Dict) ->
            case is_dict(Dict) of
                true ->
                    {ok, dict:fold(fun (Key, _Value, InnerAcc) ->
                                           Fun(Key, InnerAcc)
                                   end,
                                   Acc,
                                   Dict)};
                false ->
                    {error, undefined}
            end
    end,
    MapFold =
    fun (Fun, Acc, Dict) ->
            case is_dict(Dict) of
                true ->
                    {ok, dict:fold(fun (Key, Value, {InnerDict, InnerAcc}) ->
                                           {NewKey, NewAcc} = Fun(Key, InnerAcc),
                                           {dict:store(NewKey, Value, InnerDict), NewAcc}
                                   end,
                                   {dict:new(), Acc},
                                   Dict)};
                false ->
                    {error, undefined}
            end
    end,
    New =
    fun (_Data, _Template) ->
            dict:new()
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%% @see values/1
-spec values() -> optic:optic().
values() ->
    values(#{}).

%% @doc
%% Focus on all values of a dict.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_dict:values()],
%%             dict:from_list([{first, 1}, {second, 2}])).
%% {ok,[1,2]}
%% '''
%% @end
%% @param Options Common optic options.
%% @returns An opaque optic record.
-spec values(Options) -> optic:optic() when
      Options :: optic:variations().
values(Options) ->
    Fold =
    fun (Fun, Acc, Dict) ->
            case is_dict(Dict) of
                true ->
                    {ok, dict:fold(fun (_Key, Value, InnerAcc) ->
                                           Fun(Value, InnerAcc)
                                   end,
                                   Acc,
                                   Dict)};
                false ->
                    {error, undefined}
            end
    end,
    MapFold =
    fun (Fun, Acc, Dict) ->
            case is_dict(Dict) of
                true ->
                    {ok, dict:fold(fun (Key, Value, {InnerDict, InnerAcc}) ->
                                           {NewValue, NewAcc} = Fun(Value, InnerAcc),
                                           {dict:store(Key, NewValue, InnerDict), NewAcc}
                                   end,
                                   {dict:new(), Acc},
                                   Dict)};
                false ->
                    {error, undefined}
            end
    end,
    New =
    fun (_Data, _Template) ->
            dict:new()
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%% @see associations/1
-spec associations() -> optic:optic().
associations() ->
    associations(#{}).

%% @doc
%% Focus on all associations of a dict. An association is a tuple of
%% the key and value for each entry.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_dict:associations()],
%%             dict:from_list([{first, 1}, {second, 2}])).
%% {ok,[{first,1},{second,2}]}
%% '''
%% @end
%% @param Options Common optic options.
%% @returns An opaque optic record.
-spec associations(Options) -> optic:optic() when
      Options :: optic:variations().
associations(Options) ->
    Fold =
    fun (Fun, Acc, Dict) ->
            case is_dict(Dict) of
                true ->
                    {ok, dict:fold(fun (Key, Value, InnerAcc) ->
                                           Fun({Key, Value}, InnerAcc)
                                   end,
                                   Acc,
                                   Dict)};
                false ->
                    {error, undefined}
            end
    end,
    MapFold =
    fun (Fun, Acc, Dict) ->
            case is_dict(Dict) of
                true ->
                    {ok, dict:fold(fun (Key, Value, {InnerDict, InnerAcc}) ->
                                           {{NewKey, NewValue}, NewAcc} = Fun({Key, Value}, InnerAcc),
                                           {dict:store(NewKey, NewValue, InnerDict), NewAcc}
                                   end,
                                   {dict:new(), Acc},
                                   Dict)};
                false ->
                    {error, undefined}
            end
    end,
    New =
    fun (_Data, _Template) ->
            dict:new()
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%% @see key/2
-spec key(Key) -> optic:optic() when
    Key :: term().
key(Key) ->
    key(Key, #{}).

%% @doc
%% Focus on the value of a dict key.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_dict:key(first)],
%%             dict:from_list([{first, 1}, {second, 2}])).
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
    fun (Fun, Acc, Dict) ->
            case is_dict(Dict) of
                true ->
                    case dict:find(Key, Dict) of
                        {ok, Value} ->
                            {ok, Fun(Value, Acc)};
                        error ->
                            {error, undefined}
                    end;
                false ->
                    {error, undefined}
            end
    end,
    MapFold =
    fun (Fun, Acc, Dict) ->
            case is_dict(Dict) of
                true ->
                    case dict:find(Key, Dict) of
                        {ok, Value} ->
                            {NewValue, NewAcc} = Fun(Value, Acc),
                            {ok, {dict:store(Key, NewValue, Dict), NewAcc}};
                        error ->
                            {error, undefined}
                    end;
                false ->
                    {error, undefined}
            end
    end,
    New =
    fun (Dict, Template) ->
            case is_dict(Dict) of
                true ->
                    dict:store(Key, Template, Dict);
                false ->
                    dict:from_list([{Key, Template}])
            end
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%% @see association/2
-spec association(Key) -> optic:optic() when
      Key :: term().
association(Key) ->
    association(Key, #{}).

%% @doc
%% Focus on the association for a dict key. An association is the
%% tuple of a dict key and value. If the key is modified, the optic is
%% no longer well behaved.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_dict:association(first)],
%%             dict:from_list([{first, 1}, {second, 2}])).
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
    fun (Fun, Acc, Dict) ->
            case is_dict(Dict) of
                true ->
                    case dict:find(Key, Dict) of
                        {ok, Value} ->
                            {ok, Fun({Key, Value}, Acc)};
                        error ->
                            {error, undefined}
                    end;
                false ->
                    {error, undefined}
            end
    end,
    MapFold =
    fun (Fun, Acc, Dict) ->
            case is_dict(Dict) of
                true ->
                    case dict:find(Key, Dict) of
                        {ok, Value} ->
                            {{NewKey, NewValue}, NewAcc} = Fun({Key, Value}, Acc),
                            {ok, {dict:store(NewKey, NewValue, dict:erase(Key, Dict)), NewAcc}};
                        error ->
                            {error, undefined}
                    end;
                false ->
                    {error, undefined}
            end
    end,
    New =
    fun (Dict, Template) ->
            case is_dict(Dict) of
                true ->
                    dict:store(Key, Template, Dict);
                false ->
                    dict:from_list([{Key, Template}])
            end
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

is_dict(Unknown) ->
    try dict:size(Unknown) of
        _ ->
            true
    catch
        error:function_clause ->
            false
    end.
