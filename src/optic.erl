%%%-------------------------------------------------------------------
%%% @doc
%%% A library for creating "optics", a composable traversal over
%%% arbitrary containers with the possibility of error.
%%%
%%% The traversal is modeled as an opaque type containing both a fold
%%% and mapfold type. As mapfold is a superset of fold, fold is
%%% implemented for efficiency only. The usual fold and mapfold return
%%% types are wrapped in ok/error tuples to represent the possibility
%%% of failure, with the provided compositions being responsible for
%%% propagating errors back out and skipping further execution.
%%%
%%% These optics can then be composed to read and update nested data
%%% structures. Three types of composition are possible. A wrap, which
%%% modifies an existing optic into a new form. A chain, which
%%% combines two optics so that one focuses on the value the previous
%%% focuses on. Finally, a merge, which combines two optics to allow
%%% both to focus over the same data.
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
-type callback_new() :: fun((Data :: term(),
                             Template :: term()) ->
                         NewData :: term()).
%% Callback function invoked to create new containers.
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
-type optic_wrap(Over) :: fun((Over) -> Over).
%% A mapping function to wrap optics.
-type optic_wrap_fold() :: optic_wrap(optic_fold()).
%% A mapping function over optic folds.
-type optic_wrap_mapfold() :: optic_wrap(optic_mapfold()).
%% A mapping function over optic mapfolds.

-type variations() :: #{create=>term(),
                        strict=>boolean(),
                        filter=>callback_filter(),
                        require=>callback_filter()} | proplists:proplist().
%% Shared options to control optic construction.

-opaque optic() :: #optic{}.
%% A composable traversal over an arbitrary container.
-type optics() :: [optic()].
%% A list of traversals to be composed.

%% API
-export([% Optic creation.
         new/1,
         new/2,
         wrap/2,
         wrap/3,
         chain/1,
         merge/1,
         is_optic/1,
         variations/3,
         create/3,
         lax/1,
         % Optic application.
         fold/4,
         get/2,
         mapfold/4,
         map/3,
         put/3,
         % Optics.
         id/0,
         error/1,
         filter/1,
         require/1]).

-export_type([option/1,
              callback_map/0,
              callback_fold/0,
              callback_mapfold/0,
              callback_filter/0,
              optic_fold/0,
              optic_mapfold/0,
              optic_wrap_fold/0,
              optic_wrap_mapfold/0,
              variations/0,
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
-spec new(MapFold) -> optic() when
      MapFold :: optic_mapfold().
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
-spec new(MapFold, Fold) -> optic() when
      MapFold :: optic_mapfold(),
      Fold :: optic_fold().
new(MapFold, Fold) ->
    #optic{fold=Fold, mapfold=MapFold}.

%% @doc
%% Wrap an existing optic.
%%
%% This is the less efficient form of optic construction and will
%% infer a fold wrapper from the given mapfold wrapper.
%% @end
%% @returns An opaque optic record.
%% @see wrap/3
-spec wrap(Optic, WrapMapFold) -> optic() when
      Optic :: optic(),
      WrapMapFold :: optic_wrap_mapfold().
wrap(#optic{} = Optic, WrapMapFold) ->
    WrapFold =
    fun (Fold) ->
            MapFold = 
            fun (Fun, Acc, Data) ->
                    case Fold(Fun, Acc, Data) of
                        {ok, NewAcc} ->
                            {ok, {Data, NewAcc}};
                        {error, _} = Error ->
                            Error
                    end
            end,
            WrappedMapFold = WrapMapFold(MapFold),
            fun (Fun, Acc, Data) ->
                    case WrappedMapFold(Fun, Acc, Data) of
                        {ok, {_NewData, NewAcc}} ->
                            {ok, NewAcc};
                        {error, _} = Error ->
                            Error
                    end
            end
    end,
    wrap(Optic, WrapMapFold, WrapFold).

%% @doc
%% Wrap an existing optic.
%%
%% This allows for modifying or replacing the methods of an existing
%% optic by applying a mapping function to each of the mapfold and
%% fold methods.
%% @end
%% @param Optic An existing optic to modify.
%% @param WrapMapFold 
%% A mapping function to apply to the optic's mapfold function.
%% @end
%% @param WrapFold 
%% A mapping function to apply to the optic's fold function.
%% @end
%% @returns An opaque optic record.
-spec wrap(Optic, WrapMapFold, WrapFold) -> optic() when
      Optic :: optic(),
      WrapMapFold :: optic_wrap_mapfold(),
      WrapFold :: optic_wrap_fold().
wrap(#optic{fold=Fold, mapfold=MapFold}, WrapMapFold, WrapFold) ->
    NewMapFold = WrapMapFold(MapFold),
    NewFold = WrapFold(Fold),
    new(NewMapFold, NewFold).

%% @doc
%% Combine existing optics into a chain. In left to right order, each
%% optic then focuses on the result of the previous optic. The result
%% of this composition is itself an optic.
%%
%% This is the default composition method used for functions which
%% accept optics.
%% @end
%% @param Optics The list of optics to compose.
%% @returns An opaque optic record.
-spec chain(Optics) -> optic() when
      Optics :: optics().
chain(#optic{} = Optic) ->
    Optic;
chain([]) ->
    id();
chain([Head | Tail]) ->
    lists:foldl(fun compose/2, Head, Tail).

%% @doc
%% Merge existing optics into a single optic. In left to right order,
%% each optic focuses on the same data. The result of this composition
%% is itself an optic.
%%
%% It is the optic product type.
%% @end
%% @param Optics The list of optics to compose.
%% @returns An opaque optic record.
-spec merge(Optics) -> optic() when
      Optics :: optics().
merge(#optic{} = Optic) ->
    Optic;
merge([]) ->
    id();
merge([Head | Tail]) ->
    lists:foldl(fun product/2, Head, Tail).

%% @doc
%% Check if a term is an optic.
%% @end
%% @param Candidate The term to test.
%% @returns A boolean flag.
-spec is_optic(Candidate) -> boolean() when
      Candidate :: term().
is_optic(#optic{}) ->
    true;
is_optic(_) ->
    false.

%% @private
%% @doc
%% Internal interface for generating optics which support a range of
%% optional behaviours. The optic must return {error, undefined} when
%% it encounters an unknown type for these options to work. Due to the
%% restrictions this places on optic behaviour it is intended only for
%% internal use.
%%
%% Optics with the "create" option enabled are not well behaved, and
%% may exhibit unexpected behaviour when composed. It is also possible
%% for "filter" and "require" to no longer be well behaved, depending
%% on the filter function used.
%% @end
%% @param Optic The base optic to modify.
%% @param Options
%% The selected options. Expected options are a boolean "strict" for
%% if type errors should be reported or ignored, an arbitrarily valued
%% "create" for if type errors should force container creation, a
%% "filter" function to restrict the elements selected and a "require"
%% function to error when requirements are not met.
%% @param New
%% When the "create" option is selected, the function to invoke to
%% perform the creation.
%% @end
%% @returns An opaque optic record.
%% @see create/3
%% @see filter/1
%% @see lax/1
%% @see require/1
-spec variations(Optic, Options, New) -> optic() when
      Optic :: optic(),
      Options :: variations(),
      New :: callback_new().
variations(#optic{} = Optic, Options, New) when is_list(Options) ->
    % Normalize proplist option form to map form.
    Strict = proplists:get_bool(strict, Options),
    RequiredOptions = #{strict=>Strict},
    OptionalOptions =
    lists:foldl(
      fun (Option, Acc) ->
              case proplists:lookup(Option, Options) of
                  {Option, Value} ->
                      Acc#{Option=>Value};
                  none ->
                      Acc
              end
      end,
      RequiredOptions,
      [create, filter, require]),
    variations(Optic, OptionalOptions, New);
variations(#optic{} = Optic, #{} = Options, New) ->
    CreateOptic =
    case maps:is_key(create, Options) of
        true ->
            #{create:=Template} = Options,
            create(Optic, New, Template);
        false ->
            Optic
    end,
    LaxOptic =
    case maps:get(strict, Options, false) of
        true ->
            CreateOptic;
        false ->
            lax(CreateOptic);
        InvalidStrict ->
            erlang:error({invalid_strict_value, InvalidStrict})
    end,
    RequireOptic =
    case maps:get(require, Options, undefined) of
        undefined ->
            LaxOptic;
        Require when is_function(Require) ->
            chain([LaxOptic, require(Require)]);
        InvalidRequire ->
            erlang:error({invalid_require_value, InvalidRequire})
    end,
    FilterOptic =
    case maps:get(filter, Options, undefined) of
        undefined ->
            RequireOptic;
        Filter when is_function(Filter) ->
            chain([RequireOptic, filter(Filter)]);
        InvalidFilter ->
            erlang:error({invalid_filter_value, InvalidFilter})
    end,
    FilterOptic.

%% @doc
%% Wrap an existing optic to cause it to create a new container when
%% the optic would otherwise return `{error, undefined}' or
%% `{error, required}' during a mapfold operation.
%% @end
%% @param Optic The existing optic to wrap.
%% @param New 
%% The callback function to apply when the mapfold fails.
%% Must take two arguments, the existing data and a template argument
%% to use to populate the new data. Should return the new container,
%% which will immediately have the wrapped mapfold function re-applied
%% after creation.
%% @end
%% @param Template 
%% The template value to be given to the callback function.
%% @end
%% @returns An opaque optic record.
-spec create(Optic, New, Template) -> optic() when
      Optic :: optic(),
      New :: callback_new(),
      Template :: term().
create(Optic, New, Template) ->
    WrapFold = fun (Fold) -> Fold end,
    WrapMapFold =
    fun (MapFold) ->
            fun (Fun, Acc, Data) ->
                    case MapFold(Fun, Acc, Data) of
                        {error, Reason} when Reason == undefined;
                                             Reason == required ->
                            MapFold(Fun, Acc, New(Data, Template));
                        Result ->
                            Result
                    end
            end
    end,
    optic:wrap(Optic, WrapMapFold, WrapFold).

%% @doc
%% Wrap an existing optic to cause it to skip an element when the
%% optic would otherwise return `{error, undefined}' or 
%% `{error, required}' during a fold or mapfold operation.
%% @end
%% @param Optic The existing optic to wrap.
%% @returns An opaque optic record.
-spec lax(Optic) -> optic() when
      Optic :: optic().
lax(Optic) ->
    WrapFold =
    fun (Fold) ->
            fun (Fun, Acc, Data) ->
                    case Fold(Fun, Acc, Data) of
                        {error, Reason} when Reason == undefined;
                                             Reason == required ->
                            {ok, Acc};
                        Result ->
                            Result
                    end
            end
    end,
    WrapMapFold =
    fun (MapFold) ->
            fun (Fun, Acc, Data) ->
                    case MapFold(Fun, Acc, Data) of
                        {error, Reason} when Reason == undefined;
                                             Reason == required ->
                            {ok, {Data, Acc}};
                        Result ->
                            Result
                    end
            end
    end,
    wrap(Optic, WrapMapFold, WrapFold).

%%%===================================================================
%%% API - Optic Application
%%%===================================================================

%% @doc
%% Given a list of optics, performs a recursive fold over the result
%% of focusing on the given data structure. The order of traversal is
%% determined by the optics used.
%% @end
%% @param Optics A list of optics to apply. Leftmost is applied first.
%% @param Fold
%% The callback function to invoke on the focused elements and
%% accumulator. Expected to return the modified accumulator.
%% @end
%% @param Acc The initial accumulator value.
%% @param Data The container to apply the optics to.
%% @returns
%% On success, returns a tuple of ok and the final accumulator value.
%% On failure, returns an error tuple.
%% @end
-spec fold(Optics, Fold, Acc, Data) -> option(NewAcc) when
      Optics :: optics(),
      Fold :: callback_fold(),
      Acc :: term(),
      Data :: term(),
      NewAcc :: term().
fold(Optics, Fold, Acc, Data) ->
    #optic{fold=OpticFold} = optic:chain(Optics),
    OpticFold(Fold, Acc, Data).

%% @doc
%% Given a list of optics, returns a list of the values focused on by
%% the final optic.
%% @end
%% @param Optics A list of optics to apply. Leftmost is applied first.
%% @param Data The container to apply the optics to.
%% @returns A list of the focused values.
-spec get(Optics, Data) -> option(Values) when
      Optics :: optics(),
      Data :: term(),
      Values :: [term()].
get(Optics, Data) ->
    case fold(Optics,
              fun (Elem, Acc) -> [Elem | Acc] end,
              [],
              Data) of
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
-spec mapfold(Optics, MapFold, Acc, Data) -> option({NewData, NewAcc}) when
      Optics :: optics(),
      MapFold :: callback_mapfold(),
      Acc :: term(),
      Data :: term(),
      NewData :: term(),
      NewAcc :: term().
mapfold(Optics, MapFold, Acc, Data) ->
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
-spec map(Optics, Map, Data) -> option(NewData) when
      Optics :: optics(),
      Map :: callback_map(),
      Data :: term(),
      NewData :: term().
map(Optics, Map, Data) ->
    case mapfold(Optics,
                 fun (Elem, undefined) -> {Map(Elem), undefined} end,
                 undefined,
                 Data) of
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
-spec put(Optics, Value, Data) -> option(NewData) when
      Optics :: optics(),
      Value :: term(),
      Data :: term(),
      NewData :: term().
put(Optics, Value, Data) ->
    map(Optics, fun (_) -> Value end, Data).

%%%===================================================================
%%% API - Optics
%%%===================================================================

%% @doc
%% Focus on what was given.
%%
%% This is the identity optic, it can be chained with any other optic
%% and will return the same optic.
%%
%% Example:
%%
%% ```
%% > optic:get([optic:id()], anything).
%% {ok,[anything]}
%% '''
%% @end
%% @returns An opaque optic record.
-spec id() -> optic().
id() ->
    Fold =
    fun (Fun, Acc, Data) ->
            {ok, Fun(Data, Acc)}
    end,
    new(Fold, Fold).

%% @doc
%% Always errors with the given reason.
%%
%% Example:
%%
%% ```
%% > optic:get([optic:error(reason)], anything).
%% {error, reason}
%% '''
%% @end
%% @param Reason The error description to return.
%% @returns An opaque optic record.
-spec error(Reason) -> optic:optic() when
      Reason :: term().
error(Reason) ->
    Fold =
    fun (_Fun, _Acc, _Data) ->
            {error, Reason}
    end,
    new(Fold, Fold).

%% @doc
%% Only focuses on the current data if the given filter function
%% returns true. Otherwise the data is skipped.
%%
%% Can fail to be well behaved depending on if the filter criteria is
%% part of the focus.
%%
%% Example:
%%
%% ```
%% > IsOdd = fun (Elem) -> Elem % 2 == 1 end,
%% > optic:get([optic:filter(IsOdd)], [1,2,3]).
%% {ok, [1,3]}
%% '''
%% @end
%% @param Filter
%% The filter function to invoke to determine if the element should be
%% focused. Takes the current data as an argument, returns a boolean
%% true or false.
%% @end
%% @returns An opaque optic record.
-spec filter(Filter) -> optic:optic() when
      Filter :: callback_filter().
filter(Filter) ->
    Fold =
    fun (Fun, Acc, Data) ->
            case Filter(Data) of
                true ->
                    {ok, Fun(Data, Acc)};
                false ->
                    {ok, Acc}
            end
    end,
    MapFold =
    fun (Fun, Acc, Data) ->
            case Filter(Data) of
                true ->
                    {ok, Fun(Data, Acc)};
                false ->
                    {ok, {Data, Acc}}
            end
    end,
    new(MapFold, Fold).

%% @doc
%% Only focuses on the current data if the given filter function
%% returns true. Otherwise an `{error, required}` is returned.
%%
%% Can fail to be well behaved depending on if the filter criteria is
%% part of the focus.
%%
%% Example:
%%
%% ```
%% > IsOdd = fun (Elem) -> Elem % 2 == 1 end,
%% > optic:get([optic:require(IsOdd)], [1,2,3]).
%% {error, required}
%% '''
%% @end
%% @param Filter
%% The filter function to invoke to determine if the element should be
%% focused. Takes the current data as an argument, returns a boolean
%% true or false.
%% @end
%% @returns An opaque optic record.
-spec require(Filter) -> optic:optic() when
      Filter :: callback_filter().
require(Filter) ->
    Fold =
    fun (Fun, Acc, Data) ->
            case Filter(Data) of
                true ->
                    {ok, Fun(Data, Acc)};
                false ->
                    {error, required}
            end
    end,
    MapFold =
    fun (Fun, Acc, Data) ->
            case Filter(Data) of
                true ->
                    {ok, Fun(Data, Acc)};
                false ->
                    {error, required}
            end
    end,
    new(MapFold, Fold).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

compose(#optic{fold=Fold1, mapfold=MapFold1},
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
