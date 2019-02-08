%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(optic).

-record(optic, {fold, mapfold}).

-type error() :: {error, atom()}.
-type option(Success) :: {ok, Success} | error().

-type callback_map() :: fun((term()) -> term()).
-type callback_fold() :: fun((term(), term()) -> term()).
-type callback_mapfold() :: fun((term(), term()) -> {term(), term()}).
-type optic_fold() :: fun((callback_fold(), term(), term()) -> option(term())).
-type optic_mapfold() :: fun((callback_mapfold(), term(), term()) -> option({term(), term()})).
-type optic_new() :: fun((term(), term()) -> term()).

-type extend_options() :: #{create=>term(), strict=>boolean()} |
                          [proplists:property()].

-opaque optic() :: #optic{}.
-opaque optics() :: [optic()].

%% API
-export([new/2,
         from/1,
         '%extend'/3,
         fold/4,
         get/2,
         mapfold/4,
         map/3,
         put/3]).

-export_type([callback_map/0,
              callback_fold/0,
              callback_mapfold/0,
              optic_fold/0,
              optic_mapfold/0,
              extend_options/0,
              optic/0,
              optics/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(optic_fold(), optic_mapfold()) -> optic().
new(Fold, MapFold) ->
    #optic{fold=Fold, mapfold=MapFold}.

-spec from(optics()) -> optic().
from([]) ->
    optic_generic:id();
from([Head | Tail]) ->
    lists:foldl(fun compose/2, Head, Tail).

-spec '%extend'(optic(), extend_options(), optic_new()) -> optic().
'%extend'(#optic{} = Optic, Options, New) when is_list(Options) ->
    Strict = proplists:get_bool(strict, Options),
    RequiredOptions = #{strict=>Strict},
    OptionalOptions = case proplists:lookup(create, Options) of
        {create, Template} ->
            RequiredOptions#{create=>Template};
        none ->
            RequiredOptions
    end,
    '%extend'(Optic, OptionalOptions, New);
'%extend'(#optic{fold=Fold, mapfold=MapFold}, #{create:=Template} = Options, New) ->
    NewMapFold = fun(Fun, Acc, Data) ->
        case MapFold(Fun, Acc, Data) of
            {error, undefined} ->
                MapFold(Fun, Acc, New(Data, Template));
            Result ->
                Result
        end
    end,
    NewOptions = maps:remove(create, Options),
    '%extend'(optic:new(Fold, NewMapFold), NewOptions, New);
'%extend'(#optic{} = Optic, #{strict:=true} = Options, New) ->
    NewOptions = maps:remove(strict, Options),
    '%extend'(Optic, NewOptions, New);
'%extend'(#optic{fold=Fold, mapfold=MapFold}, #{strict:=false} = Options, New) ->
    LaxFold = fun(Fun, Acc, Data) ->
        case Fold(Fun, Acc, Data) of
            {error, undefined} ->
                {ok, Acc};
            Result ->
                Result
        end
    end,
    LaxMapFold = fun(Fun, Acc, Data) ->
        case MapFold(Fun, Acc, Data) of
            {error, undefined} ->
                {ok, {Data, Acc}};
            Result ->
                Result
        end
    end,
    NewOptions = maps:remove(strict, Options),
    '%extend'(optic:new(LaxFold, LaxMapFold), NewOptions, New);
'%extend'(#optic{} = Optic, Options, _New) when map_size(Options) == 0 ->
    Optic.

-spec fold(optics(), term(), callback_fold(), term()) -> option(term()).
fold(Optics, Data, Fold, Acc) ->
    fold_maybe(Optics, Data, Fold, {ok, Acc}).

-spec get(optics(), term()) -> option([term()]).
get(Optics, Data) ->
    case fold(Optics,
              Data,
              fun (Elem, Acc) -> [Elem | Acc] end,
              []) of
        {ok, Acc} ->
            {ok, lists:reverse(Acc)};
        {error, _} = Error ->
            Error
    end.

-spec mapfold(optics(), term(), callback_mapfold(), term()) -> option({term(), term()}).
mapfold(Optics, Data, MapFold, Acc) ->
    mapfold_maybe(Optics, Data, MapFold, {ok, Acc}).

-spec map(optics(), term(), callback_map()) -> option(term()).
map(Optics, Data, Map) ->
    case mapfold(Optics,
                 Data,
                 fun (Elem, undefined) -> {Map(Elem), undefined} end,
                 undefined) of
        {ok, {Updated, undefined}} ->
            {ok, Updated};
        {error, _} = Error ->
            Error
    end.

-spec put(optics(), term(), term()) -> option(term()).
put(Optics, Data, Value) ->
    map(Optics, Data, fun (_) -> Value end).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

compose(#optic{fold=Fold1, mapfold=MapFold1},
        #optic{fold=Fold2, mapfold=MapFold2}) ->
    Fold = fun (Fun, Acc, Data) ->
        case Fold1(Fun, Acc, Data) of
            {ok, NewAcc} ->
                Fold2(Fun, NewAcc, Data);
            {error, _} = Error ->
                Error
        end
    end,
    MapFold = fun (Fun, Acc, Data) ->
        case MapFold1(Fun, Acc, Data) of
            {ok, {NewData, NewAcc}} ->
                MapFold2(Fun, NewAcc, NewData);
            {error, _} = Error ->
                Error
        end
    end,
    optic:new(Fold, MapFold).

fold_maybe(_Optics, _Data, _Fold, {error, _} = Error) ->
    Error;
fold_maybe([], Data, Fold, {ok, Acc}) ->
    {ok, Fold(Data, Acc)};
fold_maybe([#optic{fold=undefined} | _Optics], _Data, _Fold, _Acc) ->
    {error, fold_unsupported};
fold_maybe([#optic{fold=OpticFold} | Optics], Data, Fold, {ok, _} = Acc) ->
    case OpticFold(fun (Elem, OpticAcc) ->
                       fold_maybe(Optics, Elem, Fold, OpticAcc)
                   end,
                   Acc,
                   Data) of
        {ok, {ok, _} = Result} ->
            Result;
        {ok, {error, _} = Error} ->
            Error;
        {error, _} = Error ->
            Error
    end.

mapfold_maybe(_Optics, _Data, _MapFold, {error, _} = Error) ->
    Error;
mapfold_maybe([], Data, MapFold, {ok, Acc}) ->
    {ok, MapFold(Data, Acc)};
mapfold_maybe([#optic{mapfold=undefined} | _Optics], _Data, _MapFold, _Acc) ->
    {error, mapfold_unsupported};
mapfold_maybe([#optic{mapfold=OpticMapFold} | Optics], Data, MapFold, {ok, _} = Acc) ->
    case OpticMapFold(fun (Elem, OpticAcc) ->
                          case mapfold_maybe(Optics, Elem, MapFold, OpticAcc) of
                              {ok, {NewElem, NewOpticAcc}} ->
                                  {NewElem, {ok, NewOpticAcc}};
                              {error, _} = Error ->
                                  {Elem, Error}
                          end
                      end,
                      Acc,
                      Data) of
        {ok, {NewData, {ok, NewAcc}}} ->
            {ok, {NewData, NewAcc}};
        {ok, {_, {error, _} = Error}} ->
            Error;
        {error, _} = Error ->
            Error
    end.
