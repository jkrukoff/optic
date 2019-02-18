%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions for constructing optics from lists of selectors.
%%% @end
%%%-------------------------------------------------------------------
-module(optic_path).

-type path() :: string() | binary() | non_neg_integer() | '*'.
-type paths() :: [path()].

%% API
-export([new/1]).

-export_type([path/0,
              paths/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(paths()) -> optic:optics().
new([]) ->
    [];
new([Path | Paths]) when is_list(Path); is_binary(Path) ->
    [optic_generic:key(Path) | new(Paths)];
new([Path | Paths]) when is_integer(Path), Path > 0 ->
    [optic_generic:index(Path) | new(Paths)];
new(['*' | Paths]) ->
    [optic_lists:all() | new(Paths)].
