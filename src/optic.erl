%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(optic).

-record(optic, {get, put, new, del}).

-opaque optic() :: #optic{}.
-type optics() :: [optic()].

%% API
-export([new/4,
         from/2,
         get/2,
         put/3,
         del/2]).

-export_type([optic/0]).

%%%===================================================================
%%% API
%%%===================================================================

new(Get, Put, New, Del) ->
    #optic{get=Get, put=Put, new=New, del=Del}.

from(#optic{get=Get, put=Put, new=New, del=Del}, #{} = Overrides) ->
    #{get:=NewGet, put:=NewPut, new:=NewNew, del:=NewDel} = maps:merge(
        #{get=>Get, put=>Put, new=>New, del=>Del},
        Overrides),
    new(NewGet, NewPut, NewNew, NewDel).

get([], _Data) ->
    {ok, []};
get(Optics, Data) ->
    lists:foldl(fun fold/2, {ok, [Data]}, Optics).

put([], Data, _Value) ->
    {ok, Data};
put(Optics, Data, Value) ->
    Map = fun (_) -> Value end,
    map(Optics, {ok, [Data]}, Map).

del(Optics, Data) ->
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

maybe(Options) ->
    maybe(Options, []).

maybe([], Acc) ->
    {ok, lists:append(lists:reverse(Acc))};
maybe([{error, undefined} | Options], Acc) ->
    maybe(Options, Acc);
maybe([{error, _} = Error | _Options], _Acc) ->
    Error;
maybe([{ok, Option} | Options], Acc) ->
    maybe(Options, [Option | Acc]).

fold(_Optic, {error, _} = Error) ->
    Error;
fold(#optic{get=undefined}, {ok, _}) ->
    {error, unsupported};
fold(#optic{get=Get}, {ok, Focused}) ->
    maybe([Get(Elem) || Elem <- Focused]).

map(_Optics, {error, _} = Error, _Map) ->
    Error;
map([], {ok, Focused}, Map) ->
    {ok, [Map(Elem) || Elem <- Focused]};
map([#optic{get=undefined} | _Optics], {ok, _}, _Map) ->
    {error, unsupported};
map([#optic{put=undefined} | _Optics], {ok, _}, _Map) ->
    {error, unsupported};
map([#optic{get=Get, put=Put, new=undefined} | Optics], {ok, Focused}, Map) ->
    maybe([case map(Optics, Get(Elem), Map) of
         {error, _} = Error ->
             Error;
         {ok, Value} ->
             io:format("about to put ~w ~w ~n", [Elem, Value]),
             Got = Put(Elem, Value),
             io:format("got ~w ~n", [Got]),
             Got
     end || Elem <- Focused]).
