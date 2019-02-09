%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(optic).

-record(optic, {fold :: optic_fold() | undefined,
                mapfold :: optic_mapfold() | undefined}).

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
-export([new/1,
         new/2,
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

%% @doc
%% Create a new optic for traversing a data structure.
%%
%% This is the less efficient form of optic construction and will
%% infer a fold function from the given mapfold function.
%% @end
%% @see new/2
-spec new(optic_mapfold() | undefined) -> optic().
new(MapFold) ->
    Fold = fun (Fun, Acc, Data) ->
        case MapFold(fun (Elem, InnerAcc) ->
                         {Elem, Fun(Elem, InnerAcc)}
                     end,
                     Acc,
                     Data) of
            {ok, {_, NewAcc}} ->
                {ok, NewAcc};
            {error, _} = Error ->
                Error
        end
    end,
    new(MapFold, Fold).

%% @doc
%% Create a new optic for traversing a data structure.
%%
%% At a minimum, an optic requires a mapfold function to be provided
%% for both collecting and modifying values. This function must take
%% three arguments; a callback function, an initial accumulator value
%% and an arbitrary structure to traverse. The callback function will
%% expect two values; an element and the current accumulator. It will
%% return a two item tuple with the modified element and the modified
%% accumulator. The mapfold function will return a two item tuple with
%% the modified structure and the final accumulator value, wrapped in
%% an ok or error tagged tuple.
%%
%% A fold function can also be provided for more efficient traversal
%% without modification. If one is not provided, it will be
%% inefficiently inferred from the mapfold function. This function
%% must take three arguments; a callback function, an initial
%% accumulator value and an arbitrary structure to traverse. The
%% callback function will expect two values; an element and the
%% current accumulator. It will return the modified accumulator. The
%% fold function will return a final accumulator value, wrapped in an
%% ok or error tagged tuple.
%%
%% To compose optics without unexpected side effects, the following
%% properties should hold:
%%
%% <ul>
%% <li>Get -> Put: Writing the same value as was read should result in
%% the original structure.</li>
%% <li>Put -> Get: Reading a value that was written should result in
%% the same value as was written.</li>
%% <li>Put -> Put: Writing a value twice should result in only the
%% last written value.</li>
%% </ul>
%% @end
-spec new(optic_mapfold() | undefined, optic_fold() | undefined) -> optic().
new(MapFold, undefined) ->
    new(MapFold);
new(MapFold, Fold) ->
    #optic{fold=Fold, mapfold=MapFold}.

%% @doc
%% Compose a list of optics into a single optic which traverses over
%% the same structure. This is used to combine multiple optics into a
%% single optic which traverses over the sum of all the targets.
%%
%% This is different than the composition used by the traversal
%% functions which apply the composed optics to the previous focused
%% value, instead of to the same value.
%% @end
-spec from(optics()) -> optic().
from([]) ->
    optic_generic:id();
from([Head | Tail]) ->
    lists:foldl(fun compose/2, Head, Tail).

%% @private
%% @doc
%% Internal interface for generating optics which support a range of
%% optional behaviours. The optic must return {error, undefined} when
%% it encounters an unknown type for these options to work. Due to the
%% restrictions this places on optic behaviour it is intended only for
%% internal use.
%%
%% Optics with the create option enabled are not well behaved, and may
%% exhibit unexpected behaviour when composed.
%% @end
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
    '%extend'(optic:new(NewMapFold, Fold), NewOptions, New);
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
    '%extend'(optic:new(LaxMapFold, LaxFold), NewOptions, New);
'%extend'(#optic{} = Optic, Options, _New) when map_size(Options) == 0 ->
    Optic.

%% @doc
%% Given a list of optics, performs a recursive fold over the result
%% of focusing on the given data structure.
%% @end
-spec fold(optics(), term(), callback_fold(), term()) -> option(term()).
fold(Optics, Data, Fold, Acc) ->
    fold_maybe(Optics, Data, Fold, {ok, Acc}).

%% @doc
%% Given a list of optics, returns a list of the values focused on by
%% the final optic.
%% @end
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

%% @doc
%% Given a list of optics, performs a recursive map and fold over the
%% result of focusing on the given data structure.
%% @end
-spec mapfold(optics(), term(), callback_mapfold(), term()) -> option({term(), term()}).
mapfold(Optics, Data, MapFold, Acc) ->
    mapfold_maybe(Optics, Data, MapFold, {ok, Acc}).

%% @doc
%% Given a list of optics, performs a recursive map over teh result of
%% focusing on the given data structure.
%% @end
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

%% @doc
%% Given a list of optics, modifies the values focused on by
%% the final optic.
%% @end
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
    new(MapFold, Fold).

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
