%%%-------------------------------------------------------------------
%%% @doc
%%% A set of optics specific to orddicts.
%%%
%%% As orddicts are internally represented as a list of pairs, the
%%% type checks used here are not as reliable as those used for other
%%% optics. Please ensure via other means that these optics are only
%%% used with actual orddicts.
%%% @end
%%%-------------------------------------------------------------------
-module(optic_orddict).

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
%% Focus on all keys of an orddict.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_orddict:keys()],
%%             orddict:from_list([{first, 1}, {second, 2}])).
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
            case is_orddict(Dict) of
                true ->
                    {ok, orddict:fold(fun (Key, _Value, InnerAcc) ->
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
            case is_orddict(Dict) of
                true ->
                    {ok, orddict:fold(fun (Key, Value, {InnerDict, InnerAcc}) ->
                                              {NewKey, NewAcc} = Fun(Key, InnerAcc),
                                              {orddict:store(NewKey, Value, InnerDict), NewAcc}
                                      end,
                                      {orddict:new(), Acc},
                                      Dict)};
                false ->
                    {error, undefined}
            end
    end,
    New =
    fun (_Data, _Template) ->
            orddict:new()
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%% @see values/1
-spec values() -> optic:optic().
values() ->
    values(#{}).

%% @doc
%% Focus on all values of an orddict.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_orddict:values()],
%%             orddict:from_list([{first, 1}, {second, 2}])).
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
            case is_orddict(Dict) of
                true ->
                    {ok, orddict:fold(fun (_Key, Value, InnerAcc) ->
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
            case is_orddict(Dict) of
                true ->
                    {ok, orddict:fold(fun (Key, Value, {InnerDict, InnerAcc}) ->
                                              {NewValue, NewAcc} = Fun(Value, InnerAcc),
                                              {orddict:store(Key, NewValue, InnerDict), NewAcc}
                                      end,
                                      {orddict:new(), Acc},
                                      Dict)};
                false ->
                    {error, undefined}
            end
    end,
    New =
    fun (_Data, _Template) ->
            orddict:new()
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%% @see associations/1
-spec associations() -> optic:optic().
associations() ->
    associations(#{}).

%% @doc
%% Focus on all associations of an orddict. An association is a tuple
%% of the key and value for each entry.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_orddict:associations()],
%%             orddict:from_list([{first, 1}, {second, 2}])).
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
            case is_orddict(Dict) of
                true ->
                    {ok, orddict:fold(fun (Key, Value, InnerAcc) ->
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
            case is_orddict(Dict) of
                true ->
                    {ok, orddict:fold(fun (Key, Value, {InnerDict, InnerAcc}) ->
                                              {{NewKey, NewValue}, NewAcc} = Fun({Key, Value}, InnerAcc),
                                              {orddict:store(NewKey, NewValue, InnerDict), NewAcc}
                                      end,
                                      {orddict:new(), Acc},
                                      Dict)};
                false ->
                    {error, undefined}
            end
    end,
    New =
    fun (_Data, _Template) ->
            orddict:new()
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%% @see key/2
-spec key(Key) -> optic:optic() when
      Key :: term().
key(Key) ->
    key(Key, #{}).

%% @doc
%% Focus on the value of an orddict key.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_orddict:key(first)],
%%             orddict:from_list([{first, 1}, {second, 2}])).
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
            case is_orddict(Dict) of
                true ->
                    case orddict:find(Key, Dict) of
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
            case is_orddict(Dict) of
                true ->
                    case orddict:find(Key, Dict) of
                        {ok, Value} ->
                            {NewValue, NewAcc} = Fun(Value, Acc),
                            {ok, {orddict:store(Key, NewValue, Dict), NewAcc}};
                        error ->
                            {error, undefined}
                    end;
                false ->
                    {error, undefined}
            end
    end,
    New =
    fun (Dict, Template) ->
            case is_orddict(Dict) of
                true ->
                    orddict:store(Key, Template, Dict);
                false ->
                    orddict:from_list([{Key, Template}])
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
%% Focus on the association for an orddict key. An association is the
%% tuple of a orddict key and value. If the key is modified, the optic is
%% no longer well behaved.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_orddict:association(first)],
%%             orddict:from_list([{first, 1}, {second, 2}])).
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
            case is_orddict(Dict) of
                true ->
                    case orddict:find(Key, Dict) of
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
            case is_orddict(Dict) of
                true ->
                    case orddict:find(Key, Dict) of
                        {ok, Value} ->
                            {{NewKey, NewValue}, NewAcc} = Fun({Key, Value}, Acc),
                            {ok, {orddict:store(NewKey, NewValue, orddict:erase(Key, Dict)), NewAcc}};
                        error ->
                            {error, undefined}
                    end;
                false ->
                    {error, undefined}
            end
    end,
    New =
    fun (Dict, Template) ->
            case is_orddict(Dict) of
                true ->
                    orddict:store(Key, Template, Dict);
                false ->
                    orddict:from_list([{Key, Template}])
            end
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private
%% @doc
%% Check if a container is likely to be an orddict. Is both unable to
%% disambiguate an empty list and an empty orddict, as well as only
%% inspecting the first element of the orddict in order to keep the
%% check constant time.
%% @end
is_orddict([]) ->
    true;
is_orddict([{_, _} | _]) ->
    true;
is_orddict(_) ->
    false.
