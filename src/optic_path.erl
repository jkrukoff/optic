%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions for constructing optics from lists of selectors.
%%% @end
%%%-------------------------------------------------------------------
-module(optic_path).

-type path() :: string() | binary() | non_neg_integer() | '*'.
%% A single path component.
-type paths() :: [path()].
%% A list of path components.

%% API
-export([new/1]).

-export_type([path/0,
              paths/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Construct a list of optics from a path. The type of the path
%% component determines the optic used:
%%
%% <ul>
%% <li>string: A key for a map-like structure.</li>
%% <li>binary: A key for a map-like structure.</li>
%% <li>integer: An index into a list-like structure.</li>
%% <li>'*': All elements of a list.</li>
%% </ul>
%%
%% This heavily depends on the `optic_generic' module, see the optics
%% there for the full list of containers supported.
%%
%% Example:
%%
%% ```
%% > optic:get(optic_path(["first"]), ${"first" => 1, "second" => 2}).
%% {ok,[1]}
%% '''
%% @end
%% @param Paths A list of path components to convert.
%% @returns A list of opaque optic records.
-spec new(Paths :: paths()) -> optic:optics().
new([]) ->
    [];
new([Path | Paths]) when is_list(Path); is_binary(Path) ->
    [optic_generic:key(Path) | new(Paths)];
new([Path | Paths]) when is_integer(Path), Path > 0 ->
    [optic_generic:index(Path) | new(Paths)];
new(['*' | Paths]) ->
    [optic_lists:all() | new(Paths)].
