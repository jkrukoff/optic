# optic #


### Overview ###

![Pipeline Construction](doc/pipeline.jpg)

This is an Erlang/OTP library for piping a value through a list of functions,
in the spirit of [Haskell's do
notation](https://en.wikibooks.org/wiki/Haskell/do_notation). It is
distinguished from Elixir's pipe operator "|>" by allowing for programmatic
control over whether or not each stage of the pipeline is executed.

For example, we can execute the following series of computations via a call to
one of the pipe functions:

```
> pipe:line(lists:seq(1, 100000),
            [fun (Xs) -> [X * 3 || X <- Xs] end,
             fun (Xs) -> [X || X <- Xs, X rem 2 == 1] end,
             fun lists:sum/1]).
7500000000
```

The initial value is threaded through each function given and the final result
is returned.


### Getting Started ###

This library is published to [hex.pm](https://hex.pm) as
[pipe_line](https://hex.pm/packages/pipe_line). If you're using
[rebar3](https://www.rebar3.org/) as your build tool, it can be added as a
dependency to your rebar.config as follows:

```
{deps, [{pipe_line}]}.
```

Additionally, you may wish to also include the [partial](https://github.com/jkrukoff/partial) parse transform in your
application as it cooperates well with this library.


### Usage ###

This library introduces two concepts: pipelines which execute a series of
single argument functions, and application functions which can control how
those functions are executed. These are combined to create ready made
pipelines which propagate errors.


#### Application Functions ####

Application functions are functions which take a function and a value and
decide if and how to execute the function based on the value. A set of related
error handling application functions are provided as `pipe:if_ok/2`,
`pipe:if_not_error/2`, `pipe:if_not_throw/2` and `pipe:if_not_exception/2`.
These check the value for various error forms and if found, stop executing and
instead pass the value along unchanged.

An additional class of application functions is included for adapting
functions that would not normally fit into this model. These include:
`pipe:ignore/2` for calling functions that work by side effect and
`pipe:apply/2` for calling functions that take more than a single argument.


#### Pipelines ####

Pipeline functions use the application functions to decide how to execute a
list of functions. Pre-composed versions are available for the error handling
variants.

The base pipeline function is `pipe:pipe/3`, on which all others are based.


#### Examples ####

We'll demonstrate how these functions are used by example. First, let's look
at how a sequence of transformations is represented. In order to split a set
of comma separated words and sort the result the sequence could look like
this:

```
> pipe:line("Sphinx, Of, Black, Quartz, Judge, My, Vow",
            [fun(Str) -> string:lowercase(Str) end,
             fun(Str) -> string:split(Str, ",", all) end,
             fun(Words) -> [string:strip(W, both) || W <- Words] end,
             fun(Words) -> lists:sort(Words) end]).
["black","judge","my","of","quartz","sphinx","vow"]
```

Importantly, we can see here how each stage of the pipeline is represented by
a single argument function. The `pipe:line/2` function simply passes the
result of each function along to the next stage.

Now, what about if our functions can return errors? Let's convert a unicode
string to UTF-8 and display it's bytes:

```
> Message = [90,831,842,796,807,97,775,855,858,857,108,836,844,846,814,
             103,844,842,815,793,111,777,835,841,796],
> pipe:not_error(Message,
                 [fun (Str) -> unicode:characters_to_binary(Str) end,
                  fun (Bin) -> io_lib:format("~tw", [Bin]) end,
                  fun (Formatted) -> lists:flatten(Formatted) end]).
"<<90,204,191,205,138,204,156,204,167,97,204,135,205,151,205,154,205,153,108,
205,132,205,140,205,142,204,174,103,205,140,205,138,204,175,204,153,111,204,
137,205,131,205,137,204,156>>"
```

As no errors were encountered the string was converted as expected. What about
if we pass an invalid unicode string to the same pipe?

```
> Message = [90, 55359],
> pipe:not_error(Message,
                 [fun (Str) -> unicode:characters_to_binary(Str) end,
                  fun (Bin) -> io_lib:format("~tw", [Bin]) end,
                  fun (Formatted) -> lists:flatten(Formatted) end]).
{error,<<"Z">>,[55359]}
```

Only the first function runs. The error from that function is then propagated
out as the result of the entire pipeline.

We may sometimes need to customize function application behaviour for an
individual pipeline stage. Say we want to debug the execution of our first
example by printing the intermediate values. However, `io:format/2` returns ok
instead of the value we need to continue execution. We can adapt `io:format/2`
with `pipe:via/2`, by using the `pipe:ignore/2` applicator to ignore the
return value of `io:format/2`.

```
> Inspect = pipe:via([fun pipe:ignore/2],
                     fun (Value) -> io:format("Inspect: ~tp~n", [Value]) end),
> pipe:line("Sphinx, Of, Black, Quartz, Judge, My, Vow",
            [fun(Str) -> string:lowercase(Str) end,
             Inspect,
             fun(Str) -> string:split(Str, ",", all) end,
             Inspect,
             fun(Words) -> [string:strip(W, both) || W <- Words] end,
             Inspect,
             fun(Words) -> lists:sort(Words) end,
             Inspect]).
Inspect: "sphinx, of, black, quartz, judge, my, vow"
Inspect: ["sphinx"," of"," black"," quartz"," judge"," my"," vow"]
Inspect: ["sphinx","of","black","quartz","judge","my","vow"]
Inspect: ["black","judge","my","of","quartz","sphinx","vow"]
["black","judge","my","of","quartz","sphinx","vow"]
```

We can create and compose function application for the entire pipeline as
well. For example, if we need to deal with a function like `maps:take/2` which
returns a bare error atom on failure, we could create a custom application
function like so.

```
> IfBareError = fun (Fun, Value) ->
        case Value of
                error -> Value;
                _ -> Fun(Value)
        end
  end.
```

We could then combine it with additional compatible error handling using
`pipe:pipe/3` and create a custom pipeline.

```

> pipe:pipe([fun pipe:if_not_error/2, IfBareError],
            #{key => "42"},
            [fun (Map) -> maps:take(key, Map) end,
             fun (Result) -> element(1, Result) end,
             fun (Value) -> string:to_integer(Value) end,
             fun (Result) -> element(1, Result) end]).
42
```

If we introduce an error for `maps:take/2`, we can see that the error value
propagates out of the pipeline.

```

> pipe:pipe([fun pipe:if_not_error/2, IfBareError],
            #{},
            [fun (Map) -> maps:take(key, Map) end,
             fun (Result) -> element(1, Result) end,
             fun (Value) -> string:to_integer(Value) end,
             fun (Result) -> element(1, Result) end]).
error
```

And, if we introduce an error for `string:to_integer/2`, we can see it's error
value also propagates out of the pipeline.

```
> pipe:pipe([fun pipe:if_not_error/2, IfBareError],
            #{key => "not a number"},
            [fun (Map) -> maps:take(key, Map) end,
             fun (Result) -> element(1, Result) end,
             fun (Value) -> string:to_integer(Value) end,
             fun (Result) -> element(1, Result) end]).
{error, no_integer}
```

This composition works because we continue to narrow the cases that are
allowed to continue through our function applicators. If we'd instead used
`pipe:if_ok/2` instead of `pipe:if_not_error/2`, our custom function
application would never even get the chance to run since the initial value
returned from `maps:take/2` is not of the form `{ok, _}` and execution would
have been skipped immediately.


### Partial Function Application ###

The [partial](https://github.com/jkrukoff/partial) library provides
a parse transform that can make use of this library more convenient. For
example, with the partial library the first pipe:line/2 example could instead
be written as:

```

> pipe:line("Sphinx, Of, Black, Quartz, Judge, My, Vow",
            [partial:cut(string:lowercase(_)),
             partial:cut(string:split(_, ",", all)),
             partial:cut(lists:map(partial:cut(string:strip(_, both)), _)),
             partial:cut(lists:sort(_))]).
["black","judge","my","of","quartz","sphinx","vow"]
```

Allowing to more directly see how arguments flow between pipeline stages.


### Contributing ###

Please fork the repo and submit a PR. Tests are run via:

```

rebar3 eunit
```

Documentation is autogenerated using edown and edoc via:

```

rebar3 as markdown edoc
```

The application has only been tested with Erlang/OTP 21 on Windows 10. Reports
of success (or failure!) on other versions and operating systems are
appreciated.


### Lineage ###

With code that has to handle a series of errors, the obvious answer of nested
case statements can quickly become quite unwieldy. Many people have already
adapted the ideas of [Haskell s do
notation](https://en.wikibooks.org/wiki/Haskell/do_notation) and [railway
oriented programming](https://fsharpforfunandprofit.com/rop/) to Erlang/OTP as
a way to handle errors across a sequence of operations. I decided to attempt
the same as a standalone library with no syntactic changes and instead
implement partial function application via parse transform as a separate
optional library.

[Erlando](https://github.com/rabbitmq/erlando) and
[Datum](https://github.com/fogfish/datum) were both useful resources in
understanding how to adapt haskell's monads to a dynamically typed language.
However, this [mailing list
discussion](http://erlang.org/pipermail/erlang-questions/2015-July/085109.md)
ended up being closest to what I ultimately decided to build and hinted at how
the functionality described here could be layered.


### Attribution ###

Image by Jukka Isokoski

CC BY-SA 3.0 [`https://creativecommons.org/licenses/by-sa/3.0`](https://creativecommons.org/licenses/by-sa/3.0)


## Modules ##

<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/jkrukoff/pipe/blob/master/doc/pipe.md" class="module">pipe</a></td></tr></table>
