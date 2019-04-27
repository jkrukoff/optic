%%%-------------------------------------------------------------------
%%% @doc
%%% A set of optics specific to gb_trees.
%%% @end
%%%-------------------------------------------------------------------
-module(optic_gb_trees).

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
%% Focus on all keys of a gb_tree.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_gb_trees:keys()],
%%             gb_trees:from_orddict([{first, 1}, {second, 2}])).
%% {ok,[first,second]}
%% '''
%% @end
%% @param Options Common optic options.
%% @returns An opaque optic record.
-spec keys(Options) -> optic:optic() when
      Options :: optic:variations().
keys(Options) ->
    Fold =
    fun (Fun, Acc, Tree) ->
            case is_gb_tree(Tree) of
                true ->
                    {ok, fold(fun (Key, _Value, InnerAcc) ->
                                      Fun(Key, InnerAcc)
                              end,
                              Acc,
                              Tree)};
                false ->
                    {error, undefined}
            end
    end,
    MapFold =
    fun (Fun, Acc, Tree) ->
            case is_gb_tree(Tree) of
                true ->
                    {ok, fold(fun (Key, Value, {InnerTree, InnerAcc}) ->
                                      {NewKey, NewAcc} = Fun(Key, InnerAcc),
                                      {gb_trees:enter(NewKey, Value, InnerTree), NewAcc}
                              end,
                              {gb_trees:empty(), Acc},
                              Tree)};
                false ->
                    {error, undefined}
            end
    end,
    New =
    fun (_Data, _Template) ->
            gb_trees:empty()
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%% @see values/1
-spec values() -> optic:optic().
values() ->
    values(#{}).

%% @doc
%% Focus on all values of a gb_tree.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_gb_trees:values()],
%%             gb_trees:from_orddict([{first, 1}, {second, 2}])).
%% {ok,[1,2]}
%% '''
%% @end
%% @param Options Common optic options.
%% @returns An opaque optic record.
-spec values(Options) -> optic:optic() when
      Options :: optic:variations().
values(Options) ->
    Fold =
    fun (Fun, Acc, Tree) ->
            case is_gb_tree(Tree) of
                true ->
                    {ok, fold(fun (_Key, Value, InnerAcc) ->
                                      Fun(Value, InnerAcc)
                              end,
                              Acc,
                              Tree)};
                false ->
                    {error, undefined}
            end
    end,
    MapFold =
    fun (Fun, Acc, Tree) ->
            case is_gb_tree(Tree) of
                true ->
                    {ok, fold(fun (Key, Value, {InnerTree, InnerAcc}) ->
                                      {NewValue, NewAcc} = Fun(Value, InnerAcc),
                                      {gb_trees:enter(Key, NewValue, InnerTree), NewAcc}
                              end,
                              {gb_trees:empty(), Acc},
                              Tree)};
                false ->
                    {error, undefined}
            end
    end,
    New =
    fun (_Data, _Template) ->
            gb_trees:empty()
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%% @see associations/1
-spec associations() -> optic:optic().
associations() ->
    associations(#{}).

%% @doc
%% Focus on all associations of a gb_tree. An association is a tuple of
%% the key and value for each entry.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_gb_trees:associations()],
%%             gb_trees:from_orddict([{first, 1}, {second, 2}])).
%% {ok,[{first,1},{second,2}]}
%% '''
%% @end
%% @param Options Common optic options.
%% @returns An opaque optic record.
-spec associations(Options) -> optic:optic() when
      Options :: optic:variations().
associations(Options) ->
    Fold =
    fun (Fun, Acc, Tree) ->
            case is_gb_tree(Tree) of
                true ->
                    {ok, fold(fun (Key, Value, InnerAcc) ->
                                      Fun({Key, Value}, InnerAcc)
                              end,
                              Acc,
                              Tree)};
                false ->
                    {error, undefined}
            end
    end,
    MapFold =
    fun (Fun, Acc, Tree) ->
            case is_gb_tree(Tree) of
                true ->
                    {ok, fold(fun (Key, Value, {InnerTree, InnerAcc}) ->
                                      {{NewKey, NewValue}, NewAcc} = Fun({Key, Value}, InnerAcc),
                                      {gb_trees:enter(NewKey, NewValue, InnerTree), NewAcc}
                              end,
                              {gb_trees:empty(), Acc},
                              Tree)};
                false ->
                    {error, undefined}
            end
    end,
    New =
    fun (_Data, _Template) ->
            gb_trees:empty()
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%% @see key/2
-spec key(Key) -> optic:optic() when
      Key :: term().
key(Key) ->
    key(Key, #{}).

%% @doc
%% Focus on the value of a gb_tree key.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_gb_trees:key(first)],
%%             gb_trees:from_orddict([{first, 1}, {second, 2}])).
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
    fun (Fun, Acc, Tree) ->
            case is_gb_tree(Tree) of
                true ->
                    case gb_trees:lookup(Key, Tree) of
                        {value, Value} ->
                            {ok, Fun(Value, Acc)};
                        none ->
                            {error, undefined}
                    end;
                false ->
                    {error, undefined}
            end
    end,
    MapFold =
    fun (Fun, Acc, Tree) ->
            case is_gb_tree(Tree) of
                true ->
                    case gb_trees:lookup(Key, Tree) of
                        {value, Value} ->
                            {NewValue, NewAcc} = Fun(Value, Acc),
                            {ok, {gb_trees:enter(Key, NewValue, Tree), NewAcc}};
                        none ->
                            {error, undefined}
                    end;
                false ->
                    {error, undefined}
            end
    end,
    New =
    fun (Tree, Template) ->
            case is_gb_tree(Tree) of
                true ->
                    gb_trees:enter(Key, Template, Tree);
                false ->
                    gb_trees:from_orddict([{Key, Template}])
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
%% Focus on the association for a gb_tree key. An association is the
%% tuple of a gb_tree key and value. If the key is modified, the optic is
%% no longer well behaved.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_gb_trees:association(first)],
%%             gb_trees:from_orddict([{first, 1}, {second, 2}])).
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
    fun (Fun, Acc, Tree) ->
            case is_gb_tree(Tree) of
                true ->
                    case gb_trees:lookup(Key, Tree) of
                        {value, Value} ->
                            {ok, Fun({Key, Value}, Acc)};
                        none ->
                            {error, undefined}
                    end;
                false ->
                    {error, undefined}
            end
    end,
    MapFold =
    fun (Fun, Acc, Tree) ->
            case is_gb_tree(Tree) of
                true ->
                    case gb_trees:lookup(Key, Tree) of
                        {value, Value} ->
                            {{NewKey, NewValue}, NewAcc} = Fun({Key, Value}, Acc),
                            {ok, {gb_trees:enter(NewKey, NewValue, gb_trees:delete(Key, Tree)), NewAcc}};
                        none ->
                            {error, undefined}
                    end;
                false ->
                    {error, undefined}
            end
    end,
    New =
    fun (Tree, Template) ->
            case is_gb_tree(Tree) of
                true ->
                    gb_trees:enter(Key, Template, Tree);
                false ->
                    gb_trees:from_orddict([{Key, Template}])
            end
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

fold(Fun, Acc0, Tree) ->
    List = gb_trees:to_list(Tree),
    lists:foldl(fun ({Key, Value}, Acc) ->
                        Fun(Key, Value, Acc)
                end,
                Acc0,
                List).

is_gb_tree(Unknown) ->
    try gb_trees:size(Unknown) of
        _ ->
            true
    catch
        error:function_clause ->
            false
    end.
