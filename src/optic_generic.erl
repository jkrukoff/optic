%%%-------------------------------------------------------------------
%%% @doc
%%% A set of generic optics that can be applied to multiple container
%%% types.
%%%
%%% Intended both as a convenience and to support optic creation from
%%% parsed paths in optic_path.
%%%
%%% Because of the ambiguous types they support, these optics do not
%%% support the standard optic options. Instead, they always skip
%%% unexpected types and never create missing values.
%%% @end
%%%-------------------------------------------------------------------
-module(optic_generic).

%% API
-export([key/1,
         index/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Focus on the value of many different key/value like mappings.
%% Understands how to focus on maps, property lists, dicts, orddicts
%% and gb_trees. Does not support the usual optic options.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_generic:key(first)], #{first => 1}).
%% {ok,[1]}
%% '''
%% @end
%% @param Key The key to focus on.
%% @returns An opaque optic record.
-spec key(Key) -> optic:optic() when
      Key :: term().
key(Key) ->
    Fold =
    fun (Fun, Acc, Map) when is_map(Map) ->
            case Map of
                #{Key:=Value} ->
                    {ok, Fun(Value, Acc)};
                _ ->
                    {ok, Acc}
            end;
        (Fun, Acc, List) when is_list(List) ->
            % This might be a proplist or an orddict.
            Values = proplists:get_all_values(Key, List),
            {ok, lists:foldl(Fun, Acc, Values)};
        (Fun, Acc, Unknown) ->
            case {is_dict(Unknown), is_gb_tree(Unknown)} of
                {true, _} ->
                    % It's bad if is_dict & is_gb_tree ever both return
                    % true, but I can't justify throwing an error when
                    % this is already doing such fuzzy guessing of types.
                    Dict = Unknown,
                    case dict:find(Key, Dict) of
                        {ok, Value} ->
                            {ok, Fun(Value, Acc)};
                        error ->
                            {ok, Acc}
                    end;
                {false, true} ->
                    Tree = Unknown,
                    case gb_trees:lookup(Key, Tree) of
                        {value, Value} ->
                            {ok, Fun(Value, Acc)};
                        none ->
                            {ok, Acc}
                    end;
                {false, false} ->
                    {ok, Acc}
            end
    end,
    MapFold =
    fun (Fun, Acc, Map) when is_map(Map) ->
            case Map of
                #{Key:=Value} ->
                    {NewValue, NewAcc} = Fun(Value, Acc),
                    {ok, {Map#{Key:=NewValue}, NewAcc}};
                _ ->
                    {ok, {Map, Acc}}
            end;
        (Fun, Acc, List) when is_list(List) ->
            % This might be a proplist or an orddict.
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
                                List)};
        (Fun, Acc, Unknown) ->
            case {is_dict(Unknown), is_gb_tree(Unknown)} of
                {true, _} ->
                    % It's bad if is_dict & is_gb_tree ever both return
                    % true, but I can't justify throwing an error when
                    % this is already doing such fuzzy guessing of types.
                    Dict = Unknown,
                    case dict:find(Key, Dict) of
                        {ok, Value} ->
                            {NewValue, NewAcc} = Fun(Value, Acc),
                            {ok, {dict:store(Key, NewValue, Dict), NewAcc}};
                        error ->
                            {ok, {Dict, Acc}}
                    end;
                {false, true} ->
                    Tree = Unknown,
                    case gb_trees:lookup(Key, Tree) of
                        {value, Value} ->
                            {NewValue, NewAcc} = Fun(Value, Acc),
                            {ok, {gb_trees:update(Key, NewValue, Tree), NewAcc}};
                        none ->
                            {ok, {Tree, Acc}}
                    end;
                {false, false} ->
                    {ok, {Unknown, Acc}}
            end
    end,
    optic:new(MapFold, Fold).

%% @doc
%% Focus on an element of a list like container. Indexing begins at 1.
%% Understands how to focus on lists and tuples. Does not support the
%% usual optic options.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_generic:index(3)], [1, 2, 3]).
%% {ok,[3]}
%% '''
%% @end
%% @param Index The one based index of the element to focus on.
%% @returns An opaque optic record.
-spec index(Index) -> optic:optic() when
      Index :: non_neg_integer().
index(Index) ->
    Fold =
    fun (Fun, Acc, List) when Index =< length(List) ->
            Elem = lists:nth(Index, List),
            {ok, Fun(Elem, Acc)};
        (Fun, Acc, Tuple) when Index =< tuple_size(Tuple) ->
            Elem = erlang:element(Index, Tuple),
            {ok, Fun(Elem, Acc)};
        (_Fun, Acc, _Data) ->
            {ok, Acc}
    end,
    MapFold =
    fun (Fun, Acc, List) when Index =< length(List) ->
            {Before, [Head | Tail]} = lists:split(Index - 1, List),
            {NewHead, NewAcc} = Fun(Head, Acc),
            {ok, {Before ++ [NewHead] ++ Tail, NewAcc}};
        (Fun, Acc, Tuple) when Index =< tuple_size(Tuple) ->
            Elem = erlang:element(Index, Tuple),
            {NewElem, NewAcc} = Fun(Elem, Acc),
            {ok, {erlang:setelement(Index, Tuple, NewElem), NewAcc}};
        (_Fun, Acc, Unknown) ->
            {ok, {Unknown, Acc}}
    end,
    optic:new(MapFold, Fold).

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

is_gb_tree(Unknown) ->
    try gb_trees:size(Unknown) of
        _ ->
            true
    catch
        error:function_clause ->
            false
    end.
