%%%-------------------------------------------------------------------
%%% @doc
%%% A set of optics specific to gb_sets.
%%% @end
%%%-------------------------------------------------------------------
-module(optic_gb_sets).

%% API
-export([all/0,
         all/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% @see all/1
-spec all() -> optic:optic().
all() ->
    all(#{}).

%% @doc
%% Focus on all elements of a gb_set.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_gb_sets:all()], gb_sets:from_list([1,2,3])).
%% {ok,[1,2,3]}
%% '''
%% @end
%% @param Options Common optic options.
%% @returns An opaque optic record.
-spec all(Options) -> optic:optic() when
      Options :: optic:variations().
all(Options) ->
    Fold =
    fun (Fun, Acc, Set) ->
            case is_gb_set(Set) of
                true ->
                    {ok, gb_sets:fold(Fun, Acc, Set)};
                false ->
                    {error, undefined}
            end
    end,
    MapFold =
    fun (Fun, Acc, Set) ->
            case is_gb_set(Set) of
                true ->
                    {ok, gb_sets:fold(fun (Elem, {InnerSet, InnerAcc}) ->
                                              {NewElem, NewAcc} = Fun(Elem, InnerAcc),
                                              {gb_sets:add_element(NewElem, InnerSet), NewAcc}
                                      end,
                                      {gb_sets:new(), Acc},
                                      Set)};
                false ->
                    {error, undefined}
            end
    end,
    New =
    fun (_Data, _Template) ->
            gb_sets:new()
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

is_gb_set(Unknown) ->
    try gb_sets:size(Unknown) of
        _ ->
            true
    catch
        error:function_clause ->
            false
    end.
