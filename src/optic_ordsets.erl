%%%-------------------------------------------------------------------
%%% @doc
%%% A set of optics specific to ordsets.
%%%
%%% As ordsets are internally represented as a plain list, the type
%%% checks used here are not as reliable as those used for other
%%% optics. Please ensure via other means that these optics are only
%%% used with actual ordsets.
%%% @end
%%%-------------------------------------------------------------------
-module(optic_ordsets).

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
%% Focus on all elements of an ordset.
%%
%% Example:
%%
%% ```
%% > optic:get([optic_ordsets:all()], ordsets:from_list([1,2,3])).
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
            case is_ordset(Set) of
                true ->
                    {ok, ordsets:fold(Fun, Acc, Set)};
                false ->
                    {error, undefined}
            end
    end,
    MapFold =
    fun (Fun, Acc, Set) ->
            case is_ordset(Set) of
                true ->
                    {ok, ordsets:fold(fun (Elem, {InnerSet, InnerAcc}) ->
                                              {NewElem, NewAcc} = Fun(Elem, InnerAcc),
                                              {ordsets:add_element(NewElem, InnerSet), NewAcc}
                                      end,
                                      {ordsets:new(), Acc},
                                      Set)};
                false ->
                    {error, undefined}
            end
    end,
    New =
    fun (_Data, _Template) ->
            ordsets:new()
    end,
    Optic = optic:new(MapFold, Fold),
    optic:variations(Optic, Options, New).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

is_ordset(Unknown) when is_list(Unknown) ->
    true;
is_ordset(_Unknown) ->
    false.
