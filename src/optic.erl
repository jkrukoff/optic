%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(optic).

-record(optic, {fold :: optic_fold() | undefined,
                mapfold :: optic_mapfold() | undefined}).

-type error() :: {error, term()}.
%% The expected error format.
-type option(Success) :: {ok, Success} | error().
%% A result type for operations that might fail.

-type callback_map() :: fun((Elem :: term()) -> NewElem :: term()).
%% Callback function invoked by map for each element of a container.
-type callback_fold() :: fun((Elem :: term(),
                              NewAcc :: term()) ->
                             Acc :: term()).
%% Callback function invoked by fold for each element of a container.
-type callback_mapfold() :: fun((Elem :: term(),
                                 Acc :: term()) ->
                                {NewElem :: term(), NewAcc :: term()}).
%% Callback function invoked by mapfold for each element of a
%% container.
-type optic_fold() :: fun((Fold :: callback_fold(),
                           Acc :: term(),
                           Data :: term()) ->
                          option(NewAcc :: term())).
%% A fold function to be used as part of an optic.
-type optic_mapfold() :: fun((MapFold :: callback_mapfold(),
                              Acc :: term(),
                              Data :: term()) ->
                             option({NewData :: term(), NewAcc :: term()})).
%% A mapfold function to be used as part of an optic.
-type optic_new() :: fun((Data :: term(),
                          Template :: term()) ->
                         NewData :: term()).
%% An internal interface for creating new containers.

-type extend_options() :: #{create=>term(), strict=>boolean()} |
                          [proplists:property()].
%% Shared options to control optic construction.

-opaque optic() :: #optic{}.
%% A composable traversal over an arbitrary container.
-type optics() :: [optic()].
%% A list of traversals to be composed.

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
%% @returns An opaque optic record.
%% @see new/2
-spec new(MapFold :: optic_mapfold() | undefined) -> optic().
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
%% Well behaved optics should implement the following properties:
%%
%% <ul>
%% <li>Get -> Put: Writing the same value as was read should result in
%% the original structure.</li>
%% <li>Put -> Get: Reading a value that was written should result in
%% the same value as was written.</li>
%% <li>Put -> Put: Writing a value twice should result in only the
%% last written value.</li>
%% </ul>
%%
%% Optics which are not well behaved will be more difficult to use and
%% compose with other optics. Their behaviour will change depending on
%% the order in which they are applied and the number of times they
%% are applied.
%%
%% @end
%% @param MapFold
%% At a minimum, an optic requires a mapfold function to be provided
%% for both collecting and modifying values. This function must take
%% three arguments; a callback function, an initial accumulator value
%% and an arbitrary structure to traverse. The callback function will
%% expect two values; an element and the current accumulator. It will
%% return a two item tuple with the modified element and the modified
%% accumulator. The mapfold function will return a two item tuple with
%% the modified structure and the final accumulator value, wrapped in
%% an ok or error tagged tuple.
%% @end
%% @param Fold
%% A fold function can also be provided for more efficient traversal
%% without modification. If one is not provided, it will be
%% inefficiently inferred from the MapFold function. This function
%% must take three arguments; a callback function, an initial
%% accumulator value and an arbitrary structure to traverse. The
%% callback function will expect two values; an element and the
%% current accumulator. It will return the modified accumulator. The
%% fold function will return a final accumulator value, wrapped in an
%% ok or error tagged tuple.
%% @end
%% @returns An opaque optic record.
-spec new(MapFold :: optic_mapfold() | undefined, Fold :: optic_fold() | undefined) -> optic().
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
%% @param Optics
%% A list of optics to compose. Leftmost is applied first.
%% @end
%% @returns An opaque optic record.
%% @see new/2
-spec from(Optics :: optics()) -> optic().
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
%% @param Optic The base optic to modify.
%% @param Options The selected options.
%% @param New
%% When the "create" option is selected, the function to invoke to
%% perform the creation.
%% @end
%% @returns An opaque optic record.
-spec '%extend'(Optic :: optic(), Options :: extend_options(), New :: optic_new()) -> optic().
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
%% of focusing on the given data structure. The order of traversal is
%% determined by the optics used.
%% @end
%% @param Optics A list of optics to apply. Leftmost is applied first.
%% @param Data The container to apply the optics to.
%% @param Fold
%% The callback function to invoke on the focused elements and
%% accumulator. Expected to return the modified accumulator.
%% @end
%% @param Acc The initial accumulator value.
%% @returns
%% On success, returns a tuple of ok and the final accumulator value.
%% On failure, returns an error tuple.
%% @end
-spec fold(Optics :: optics(), Data :: term(), Fold :: callback_fold(), Acc :: term()) -> NewAcc :: option(term()).
fold(Optics, Data, Fold, Acc) ->
    fold_maybe(Optics, Data, Fold, {ok, Acc}).

%% @doc
%% Given a list of optics, returns a list of the values focused on by
%% the final optic.
%% @end
%% @param Optics A list of optics to apply. Leftmost is applied first.
%% @param Data The container to apply the optics to.
%% @returns A list of the focused values.
-spec get(Optics :: optics(), Data :: term()) -> option([term()]).
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
%% @param Optics A list of optics to apply. Leftmost is applied first.
%% @param Data The container to apply the optics to.
%% @param MapFold
%% The callback function to invoke on the focused elements and
%% accumulator. Expected to return a tuple of the modified element and
%% accumulator.
%% @end
%% @param Acc The initial accumulator value.
%% @returns
%% On success, returns a tuple of ok and a tuple of the modified
%% container and the final accumulator value. On failure, returns an
%% error tuple.
%% @end
-spec mapfold(Optics :: optics(), Data :: term(), MapFold :: callback_mapfold(), Acc :: term()) -> option({NewData :: term(), NewAcc :: term()}).
mapfold(Optics, Data, MapFold, Acc) ->
    mapfold_maybe(Optics, Data, MapFold, {ok, Acc}).

%% @doc
%% Given a list of optics, performs a recursive map over the result of
%% focusing on the given data structure.
%% @end
%% @param Optics A list of optics to apply. Leftmost is applied first.
%% @param Data The container to apply the optics to.
%% @param Map
%% The callback function to invoke on the focused elements. Expected
%% to return a modified element.
%% @end
%% @returns
%% On success, returns a tuple of ok and the modified container.
%% On failure, returns an error tuple.
%% @end
-spec map(Optics :: optics(), Data :: term(), Map :: callback_map()) -> option(NewData :: term()).
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
%% @param Optics A list of optics to apply. Leftmost is applied first.
%% @param Data The container to apply the optics to.
%% On success, returns a tuple of ok and the modified container.
%% On failure, returns an error tuple.
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
