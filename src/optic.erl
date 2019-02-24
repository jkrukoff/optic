%%%-------------------------------------------------------------------
%%% @doc
%%% A library for creating "optics", a composable traversal over
%%% arbitrary containers.
%%%
%%% These optics can then be composed to read and update nested data
%%% structures.
%%% @end
%%%-------------------------------------------------------------------
-module(optic).

-record(optic, {fold :: optic_fold(),
                mapfold :: optic_mapfold()}).

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
-type callback_filter() :: fun((Elem :: term()) -> boolean()).
%% Callback function invoked by filter optics for each element of a
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

-type extend_options() :: #{create=>term(),
                            strict=>boolean(),
                            filter=>callback_filter()} |
[proplists:property()].
%% Shared options to control optic construction.

-opaque optic() :: #optic{}.
%% A composable traversal over an arbitrary container.
-type optics() :: [optic()].
%% A list of traversals to be composed.

%% API
-export([% Optic creation.
         new/1,
         new/2,
         chain/1,
         merge/1,
         '%extend'/3,
         % Optic application.
         fold/4,
         get/2,
         mapfold/4,
         map/3,
         put/3]).

-export_type([callback_map/0,
              callback_fold/0,
              callback_mapfold/0,
              callback_filter/0,
              optic_fold/0,
              optic_mapfold/0,
              extend_options/0,
              optic/0,
              optics/0]).

%%%===================================================================
%%% API - Optic Creation & Composition
%%%===================================================================

%% @doc
%% Create a new optic for traversing a data structure.
%%
%% This is the less efficient form of optic construction and will
%% infer a fold function from the given mapfold function.
%% @end
%% @returns An opaque optic record.
%% @see new/2
-spec new(MapFold :: optic_mapfold()) -> optic().
new(MapFold) ->
    Fold =
    fun (Fun, Acc, Data) ->
            case MapFold(
                   fun (Elem, InnerAcc) ->
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
-spec new(MapFold :: optic_mapfold(), Fold :: optic_fold()) -> optic().
new(MapFold, Fold) ->
    #optic{fold=Fold, mapfold=MapFold}.

-spec chain(Optics :: optics()) -> optic().
chain(#optic{} = Optic) ->
    Optic;
chain([]) ->
    optic_generic:id();
chain([Head | Tail]) ->
    lists:foldl(fun sum/2, Head, Tail).

-spec merge(Optics :: optics()) -> optic().
merge(#optic{} = Optic) ->
    Optic;
merge([]) ->
    optic_generic:id();
merge([Head | Tail]) ->
    lists:foldl(fun product/2, Head, Tail).

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
%% @param Options
%% The selected options. Expected options are a boolean "strict" for
%% if type errors should be reported or ignored, an arbitrarily valued
%% "create" for if type errors should force container creation, and a
%% "filter" function to restrict the elements selected.
%% @param New
%% When the "create" option is selected, the function to invoke to
%% perform the creation.
%% @end
%% @returns An opaque optic record.
-spec '%extend'(Optic :: optic(), Options :: extend_options(), New :: optic_new()) -> optic().
'%extend'(#optic{} = Optic, Options, New) when is_list(Options) ->
 % Normalize proplist option form to map form.
 Strict = proplists:get_bool(strict, Options),
 RequiredOptions = #{strict=>Strict},
 OptionalOptions = lists:foldl(
                     fun (Option, Acc) ->
                             case proplists:lookup(Option, Options) of
                                 {Option, Value} ->
                                     Acc#{Option=>Value};
                                 none ->
                                     Acc
                             end
                     end,
                     RequiredOptions,
                     [create, filter]),
 '%extend'(Optic, OptionalOptions, New);
'%extend'(#optic{} = Optic, #{} = Options, New) ->
 CreateOptic = case maps:is_key(create, Options) of
                   true ->
                       #{create:=Template} = Options,
                       extend_create(Optic, New, Template);
                   false ->
                       Optic
               end,
 StrictOptic = case maps:get(strict, Options, false) of
                   true ->
                       CreateOptic;
                   false ->
                       extend_lax(CreateOptic);
                   InvalidStrict ->
                       throw({invalid_strict_value, InvalidStrict})
               end,
 FilterOptic = case maps:get(filter, Options, undefined) of
                   undefined ->
                       StrictOptic;
                   Filter when is_function(Filter) ->
                       extend_filter(StrictOptic, Filter);
                   InvalidFilter ->
                       throw({invalid_filter_value, InvalidFilter})
               end,
 FilterOptic.

%%%===================================================================
%%% API - Optic Application
%%%===================================================================

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
-spec fold(Optics :: optics(),
           Data :: term(),
           Fold :: callback_fold(),
           Acc :: term()) -> option(NewAcc :: term()).
fold(Optics, Data, Fold, Acc) ->
    #optic{fold=OpticFold} = optic:chain(Optics),
    OpticFold(Fold, Acc, Data).

%% @doc
%% Given a list of optics, returns a list of the values focused on by
%% the final optic.
%% @end
%% @param Optics A list of optics to apply. Leftmost is applied first.
%% @param Data The container to apply the optics to.
%% @returns A list of the focused values.
-spec get(Optics :: optics(),
          Data :: term()) -> option(Values :: [term()]).
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
-spec mapfold(Optics :: optics(),
              Data :: term(),
              MapFold :: callback_mapfold(),
              Acc :: term()) -> option({NewData :: term(), NewAcc :: term()}).
mapfold(Optics, Data, MapFold, Acc) ->
    #optic{mapfold=OpticMapFold} = optic:chain(Optics),
    OpticMapFold(MapFold, Acc, Data).

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
-spec map(Optics :: optics(),
          Data :: term(),
          Map :: callback_map()) -> option(NewData :: term()).
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
%% @returns
%% On success, returns a tuple of ok and the modified container.
%% On failure, returns an error tuple.
%% @end
-spec put(Optics :: optics(),
          Data :: term(),
          Value :: term()) -> option(NewData :: term()).
put(Optics, Data, Value) ->
    map(Optics, Data, fun (_) -> Value end).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

sum(#optic{fold=Fold1, mapfold=MapFold1},
    #optic{fold=Fold2, mapfold=MapFold2}) ->
    Fold =
    fun (Fun, Acc, Data) ->
            case Fold2(
                   fun (Elem, {ok, InnerAcc}) ->
                           Fold1(Fun, InnerAcc, Elem);
                       (_Elem, {error, _} = Error) ->
                           Error
                   end,
                   {ok, Acc},
                   Data) of
                {ok, {ok, _} = Result} ->
                    Result;
                {ok, {error, _} = Error} ->
                    Error;
                {error, _} = Error ->
                    Error
            end
    end,
    MapFold =
    fun (Fun, Acc, Data) ->
            case MapFold2(
                   fun (Elem, {ok, InnerAcc}) ->
                           case MapFold1(Fun, InnerAcc, Elem) of
                               {ok, {NewElem, NewInnerAcc}} ->
                                   {NewElem, {ok, NewInnerAcc}};
                               {error, _} = Error ->
                                   {Elem, Error}
                           end;
                       (Elem, {error, _} = Error) ->
                           {Elem, Error}
                   end,
                   {ok, Acc},
                   Data) of
                {ok, {NewData, {ok, NewAcc}}} ->
                    {ok, {NewData, NewAcc}};
                {ok, {_, {error, _} = Error}} ->
                    Error;
                {error, _} = Error ->
                    Error
            end
    end,
    new(MapFold, Fold).

product(#optic{fold=Fold1, mapfold=MapFold1},
        #optic{fold=Fold2, mapfold=MapFold2}) ->
    Fold =
    fun (Fun, Acc, Data) ->
            case Fold2(Fun, Acc, Data) of
                {ok, NewAcc} ->
                    Fold1(Fun, NewAcc, Data);
                {error, _} = Error ->
                    Error
            end
    end,
    MapFold =
    fun (Fun, Acc, Data) ->
            case MapFold2(Fun, Acc, Data) of
                {ok, {NewData, NewAcc}} ->
                    MapFold1(Fun, NewAcc, NewData);
                {error, _} = Error ->
                    Error
            end
    end,
    new(MapFold, Fold).

extend_create(#optic{fold=Fold, mapfold=MapFold}, New, Template) ->
    NewMapFold =
    fun(Fun, Acc, Data) ->
            case MapFold(Fun, Acc, Data) of
                {error, undefined} ->
                    MapFold(Fun, Acc, New(Data, Template));
                Result ->
                    Result
            end
    end,
    optic:new(NewMapFold, Fold).

extend_filter(#optic{fold=Fold, mapfold=MapFold}, Filter) ->
    FilterFold =
    fun(Fun, Acc, Data) ->
            Fold(fun (Elem, InnerAcc) ->
                         case Filter(Elem) of
                             true ->
                                 Fun(Elem, InnerAcc);
                             false ->
                                 InnerAcc
                         end
                 end,
                 Acc,
                 Data)
    end,
    FilterMapFold =
    fun(Fun, Acc, Data) ->
            MapFold(fun (Elem, InnerAcc) ->
                            case Filter(Elem) of
                                true ->
                                    Fun(Elem, InnerAcc);
                                false ->
                                    {Elem, InnerAcc}
                            end
                    end,
                    Acc,
                    Data)
    end,
    optic:new(FilterMapFold, FilterFold).

extend_lax(#optic{fold=Fold, mapfold=MapFold}) ->
    LaxFold =
    fun(Fun, Acc, Data) ->
            case Fold(Fun, Acc, Data) of
                {error, undefined} ->
                    {ok, Acc};
                Result ->
                    Result
            end
    end,
    LaxMapFold =
    fun(Fun, Acc, Data) ->
            case MapFold(Fun, Acc, Data) of
                {error, undefined} ->
                    {ok, {Data, Acc}};
                Result ->
                    Result
            end
    end,
    optic:new(LaxMapFold, LaxFold).
