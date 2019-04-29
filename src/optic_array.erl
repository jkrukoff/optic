%%%-------------------------------------------------------------------
%%% @doc
%%% A set of optics specific to arrays.
%%% @end
%%%-------------------------------------------------------------------
-module(optic_array).

%% API
-export([all/0,
         all/1,
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
%% Focus on all values of an array.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_array:all()], array:from_list([1,2,3])).
%% {ok,[1,2,3]}
%% '''
%% @end
%% @param Options Common optic options.
%% @returns An opaque optic record.
-spec all(Options) -> optic:optic() when
      Options :: optic:variations().
all(Options) ->
    Fold =
    fun (Fun, Acc, Array) ->
            case is_array(Array) of
                true ->
                    {ok, array:foldl(fun (_Index, Elem, InnerAcc) ->
                                             Fun(Elem, InnerAcc)
                                     end, Acc, Array)};
                false ->
                    {error, undefined}
            end
    end,
    MapFold =
    fun (Fun, Acc, Array) ->
            case is_array(Array) of
                true ->
                    {ok, array:foldl(fun (Index, Elem, {InnerArray, InnerAcc}) ->
                                             {NewElem, NewAcc} = Fun(Elem, InnerAcc),
                                             {array:set(Index, NewElem, InnerArray), NewAcc}
                                     end,
                                     {Array, Acc},
                                     Array)};
                false ->
                    {error, undefined}
            end
    end,
    New =
    fun (_Data, Template) ->
            array:new([{default, Template}])
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%% @see nth/2
-spec nth(N) -> optic:optic() when
      N :: pos_integer().
nth(N) ->
    nth(N, #{}).

%% @doc
%% Focus on the nth value of an array. Like lists, but unlike the
%% standard array operations, indexing begins at 1.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_array:nth(1)], array:from_list([1,2,3])).
%% {ok,[1]}
%% '''
%% @end
%% @param N The index of the array value to focus on.
%% @param Options Common optic options.
%% @returns An opaque optic record.
-spec nth(N, Options) -> optic:optic() when
      N :: pos_integer(),
      Options :: optic:variations().
nth(N, Options) when N >= 1 ->
    Index = N - 1,
    Fold =
    fun (Fun, Acc, Array) ->
            case is_array(Array) andalso valid_index(Index, Array) of
                true ->
                    Elem = array:get(Index, Array),
                    {ok, Fun(Elem, Acc)};
                false ->
                    {error, undefined}
            end
    end,
    MapFold =
    fun (Fun, Acc, Array) ->
            case is_array(Array) andalso valid_index(Index, Array) of
                true ->
                    Elem = array:get(Index, Array),
                    {NewElem, NewAcc} = Fun(Elem, Acc),
                    {ok, {array:set(Index, NewElem, Array), NewAcc}};
                false ->
                    {error, undefined}
            end
    end,
    New =
    fun (Data, Template) ->
            case is_array(Data) of
                true ->
                    array:resize(N, Data);
                false ->
                    array:new(N, [{default, Template}])
            end
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

is_array(Unknown) ->
    try array:size(Unknown) of
        _ ->
            true
    catch
        error:badarg ->
            false
    end.

valid_index(Index, Array) when Index >= 0 ->
    case array:is_fix(Array) of
        true ->
            Index < array:size(Array);
        false ->
            true
    end.
