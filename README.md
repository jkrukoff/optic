# optic #

![Camera Lenses](doc/lenses.jpg)

### Overview ###

This is an Erlang/OTP library for retrieving and modifying nested values, in
the spirit of [Haskell's lens
library](https://en.wikibooks.org/wiki/Haskell/Lenses_and_functional_references).
Functional selectors for deeply nested values are constructed by composing
"optics", each of which specifies a way to focus on a particular kind of
value.

For example, say we had a list of deserialized JSON entities representing pets
for sale that we wanted to modify.

```
> Pets = [#{
    <<"id">> => 628178654,
    <<"name">> => <<"spot">>,
    <<"status">> => <<"available">>,
    <<"category">> => #{
      <<"id">> => 3216199393,
      <<"name">> => <<"dog">>
    }}].
```

We could then update all pets to a new status by:

```
> optic:put([optic_lists:all(), optic_maps:key(<<"status">>)],
            Pets,
            <<"sold">>).
{ok,[#{
  <<"id">> => 628178654,
  <<"name">> => <<"spot">>,
  <<"status">> => <<"sold">>,
  <<"category">> => #{
    <<"id">> => 3216199393,
    <<"name">> => <<"dog">>
  }}]}
```


## Modules ##

<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/jkrukoff/optic/blob/master/doc/optic.md" class="module">optic</a></td></tr>
<tr><td><a href="http://github.com/jkrukoff/optic/blob/master/doc/optic_generic.md" class="module">optic_generic</a></td></tr>
<tr><td><a href="http://github.com/jkrukoff/optic/blob/master/doc/optic_lists.md" class="module">optic_lists</a></td></tr>
<tr><td><a href="http://github.com/jkrukoff/optic/blob/master/doc/optic_maps.md" class="module">optic_maps</a></td></tr>
<tr><td><a href="http://github.com/jkrukoff/optic/blob/master/doc/optic_path.md" class="module">optic_path</a></td></tr>
<tr><td><a href="http://github.com/jkrukoff/optic/blob/master/doc/optic_proplists.md" class="module">optic_proplists</a></td></tr>
<tr><td><a href="http://github.com/jkrukoff/optic/blob/master/doc/optic_tuples.md" class="module">optic_tuples</a></td></tr></table>


### Getting Started ###

This library is published to [hex.pm](https://hex.pm) as [optic](https://hex.pm/packages/optic). If you're using [rebar3](https://www.rebar3.org/) as your build tool, it can be added
as a dependency to your rebar.config as follows:

```
{deps, [{optic}]}.
```


### Usage ###

The library is expected to be used by first creating a list of _optics_
which represent operations on particular kinds of containers. That list can
then be used with a variety of different common operations in the library for
reading or writing values in nested data structures in different ways.


#### Optics ####

An optic is an opaque record that represents a method for traversing over and
modifying a particular container. Many different pre-created optics are
included in the library for standard Erlang containers as well as a method for
creating new optics.

The fundamental operation supported is a mapfold, though a fold may also be
implemented (and is recommended) for better performance for read only
operations. Additionally convenience functions for getting and putting static
values are included and are built on top of the more powerful primitive
operations.

Optics which are associative and idempotent are well behaved. These optics
can be easily combined in any way this library supports without surprise.
Optics which do not have these properties will require careful attention to
how they are used and the order in which they are applied.

Most of the optics in this library support the same set of options to control
how they behave. Those options are passed as a map or proplist:

* strict: When true, this causes an error to be returned when asked to
traverse over a container of an unexpected type. When false or not given,
unexpected containers will be silently skipped.

* create: When given, this causes a new container of the expected type to be
created when no container is present or when asked to traverse over a
container of an unexpected type. If the traversal type needs to create
individual values, the value of the create property is used as a template to
create the needed default objects. Setting this property causes the optic to
no longer be well behaved.

* filter: When given, expects a value as a filter function to determine if
the traversed element should be focused or skipped. The filter function should
take a single arbitrary element and return a boolean true/false. If the
criteria the filter function uses to select an element is modified, the
filtered optic will no longer be well behaved.


#### Examples ####

Let's look at some examples of how optics can be used. We'll start with a list
of maps and demonstrate various ways it can be processed:

```

> Data = [#{name => first}, #{name => second}, #{name => third}].
```

We can extract a single value using one of the lists optics to extract the
head of the list, and combine it with an optic to extract the value of the
"name" key from the map.

```

> optic:get([optic_lists:head(), optic_maps:key(name)], Data).
{ok,[first]}
```

This shows how optics are applied from left to right.

If we wanted to traverse over all the members of the list, we can simply
change the list optic used to do so:

```
> optic:get([optic_lists:all(), optic_maps:key(name)], Data).
{ok,[first,second,third]}
```

Optics can also be composed using `optic:from/1` in order to select
multiple elements at the same level. For instance, to select only the first
two maps, we could do this:

```
> FirstTwo = optic:from([optic_lists:nth(1), optic_lists:nth(2)]),
> optic:get([FirstTwo, optic_maps:key(name)], Data).
{ok,[first,second]}
```

When presented with heterogeneous data, the strict option can be used to
control how it is handled. We may want to simply skip it when such is
expected, or we may want to report an error. The only change is in how the
optic is constructed:

```
> Data = [#{name => first},#{id => second},#{name => third}].
[#{name => first},#{id => second},#{name => third}].
> optic:get([optic_lists:all(), optic_maps:key(name)], Data).
{ok,[first,third]}
> optic:get([optic_lists:all(), optic_maps:key(name, #{strict => false})], Data).
{ok,[first,third]}
> optic:get([optic_lists:all(), optic_maps:key(name, #{strict => true})], Data).
{error,undefined}
```

We can see here that we get lax handling of unexpected types by default.

The create option is closely related, as creation is only triggered in cases
where strict would have returned an error. In that case a container is instead
either constructed or modified to allow the operation to succeed.

Let's explore the variations in the context of modifying a container:

```
> Data = [#{name => first},#{id => second},#{name => third}].
[#{name => first},#{id => second},#{name => third}].
> optic:put([optic_lists:all(), optic_maps:key(name)], Data, example).
{ok,[#{name => example},#{id => second},#{name => example}]}
> optic:put([optic_lists:all(), optic_maps:key(name, #{strict=>true})], Data, example).
{error,undefined}
> optic:put([optic_lists:all(), optic_maps:key(name, #{create=>undefined})], Data, example).
{ok,[#{name => example},
     #{id => second,name => example},
     #{name => example}]}
```

The value given to create is used as a template when additional values need to
be constructed. In many cases, it is then immediately overwritten. The
behaviour is context dependent and will vary based on the optic used.

```
> optic:put([optic_lists:nth(3, #{create => #{}}),
             optic_maps:key(name, #{create => undefined})], [], example).
{ok,[#{},#{},#{name => example}]}
```

Finally, the filter option can be used to control which elements are selected
based on their values. For instance, say we wanted to only collect the status
of maps with a particular name:

```
> Data = [#{name => first, status => ready},
          #{name => second, status => ready},
          #{name => third, status => delayed}].
[#{name => first,status => ready},
 #{name => second,status => ready},
 #{name => third,status => delayed}]
> optic:get([optic_lists:all([{filter,
                               fun (Elem) -> maps:get(name, Elem) == third end}]),
             optic_maps:key(status)], Data).
{ok,[delayed]}
```

More complicated operations should refer to the `fold/4`, `map/3` and
`mapfold/4` functions to allow for values to be computed during the traversal.


#### Paths ####

For traversing JSON like structures of maps and lists, the `optic_path` module
provides a simplified interface for optic construction. It parses a list of
path components into a list of optics that can be used with any of the optic
traversal functions.

```
> Data = #{<<"result">> => [#{<<"name">> => <<"example">>}]},
> Path = optic_path:new([<<"result">>, '*', <<"name">>]),
> optic:get(Path, Data).
{ok,[<<"example">>]}
> optic:put(Path, Data, <<"modified">>).
{ok,#{<<"result">> => [#{<<"name">> => <<"modified">>}]}}
```

Notable restrictions for paths are they require map-like keys to be strings or
binaries. No provision is made for the usual optic options, instead traversal
is always lax and creation is never done.


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

I first encountered lenses via the[Datum](https://github.com/fogfish/datum) library, but wanted a
version that supported traversables instead of lenses in order to support
updating multiple elements. As lenses are a special case of traversables, this
allowed for using a single type to represent both.

There are a number of good introductions to lenses, [this](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial)
was the most accessible for me.

This library was initially conceived with the intention of making it easy to
modify deeply nested JSON, so JSON related data structures were the first
implemented.

Several generic transformations are possible on optics, but the required
expectations are not always obvious or possible to enforce. As such, the
choice was made to privilege the optics created by the library to allow for
easily creating variations, since those could be expected to be implemented
consistently.


### Attribution ###

Image by Bill Ebbesen
CC BY-SA 3.0 [`https://creativecommons.org/licenses/by/3.0/deed.en`](https://creativecommons.org/licenses/by/3.0/deed.en)
