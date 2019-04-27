

# Module optic #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

A library for creating "optics", a composable traversal over
arbitrary containers with the possibility of error.

<a name="description"></a>

## Description ##

The traversal is modeled as an opaque type containing both a fold
and mapfold type. As mapfold is a superset of fold, fold is
implemented for efficiency only. The usual fold and mapfold return
types are wrapped in ok/error tuples to represent the possibility
of failure, with the provided compositions being responsible for
propagating errors back out and skipping further execution.

These optics can then be composed to read and update nested data
structures. Three types of composition are possible. A wrap, which
modifies an existing optic into a new form. A chain, which
combines two optics so that one focuses on the value the previous
focuses on. Finally, a merge, which combines two optics to allow
both to focus over the same data.
<a name="types"></a>

## Data Types ##




### <a name="type-callback_filter">callback_filter()</a> ###


<pre><code>
callback_filter() = fun((Elem::term()) -&gt; boolean())
</code></pre>

 Callback function invoked by filter optics for each element of a
container.



### <a name="type-callback_fold">callback_fold()</a> ###


<pre><code>
callback_fold() = fun((Elem::term(), NewAcc::term()) -&gt; Acc::term())
</code></pre>

 Callback function invoked by fold for each element of a container.



### <a name="type-callback_map">callback_map()</a> ###


<pre><code>
callback_map() = fun((Elem::term()) -&gt; NewElem::term())
</code></pre>

 Callback function invoked by map for each element of a container.



### <a name="type-callback_mapfold">callback_mapfold()</a> ###


<pre><code>
callback_mapfold() = fun((Elem::term(), Acc::term()) -&gt; {NewElem::term(), NewAcc::term()})
</code></pre>

 Callback function invoked by mapfold for each element of a
container.



### <a name="type-callback_new">callback_new()</a> ###


<pre><code>
callback_new() = fun((Data::term(), Template::term()) -&gt; NewData::term())
</code></pre>

 Callback function invoked to create new containers.



### <a name="type-error">error()</a> ###


<pre><code>
error() = {error, term()}
</code></pre>

 The expected error format.



### <a name="type-optic">optic()</a> ###


__abstract datatype__: `optic()`

 A composable traversal over an arbitrary container.



### <a name="type-optic_fold">optic_fold()</a> ###


<pre><code>
optic_fold() = fun((Fold::<a href="#type-callback_fold">callback_fold()</a>, Acc::term(), Data::term()) -&gt; <a href="#type-option">option</a>(NewAcc::term()))
</code></pre>

 A fold function to be used as part of an optic.



### <a name="type-optic_mapfold">optic_mapfold()</a> ###


<pre><code>
optic_mapfold() = fun((MapFold::<a href="#type-callback_mapfold">callback_mapfold()</a>, Acc::term(), Data::term()) -&gt; <a href="#type-option">option</a>({NewData::term(), NewAcc::term()}))
</code></pre>

 A mapfold function to be used as part of an optic.



### <a name="type-optic_wrap">optic_wrap()</a> ###


<pre><code>
optic_wrap(Over) = fun((Over) -&gt; Over)
</code></pre>

 A mapping function to wrap optics.



### <a name="type-optic_wrap_fold">optic_wrap_fold()</a> ###


<pre><code>
optic_wrap_fold() = <a href="#type-optic_wrap">optic_wrap</a>(<a href="#type-optic_fold">optic_fold()</a>)
</code></pre>

 A mapping function over optic folds.



### <a name="type-optic_wrap_mapfold">optic_wrap_mapfold()</a> ###


<pre><code>
optic_wrap_mapfold() = <a href="#type-optic_wrap">optic_wrap</a>(<a href="#type-optic_mapfold">optic_mapfold()</a>)
</code></pre>

 A mapping function over optic mapfolds.



### <a name="type-optics">optics()</a> ###


<pre><code>
optics() = [<a href="#type-optic">optic()</a>]
</code></pre>

 A list of traversals to be composed.



### <a name="type-option">option()</a> ###


<pre><code>
option(Success) = {ok, Success} | <a href="#type-error">error()</a>
</code></pre>

 A result type for operations that might fail.



### <a name="type-variations">variations()</a> ###


<pre><code>
variations() = #{create =&gt; term(), strict =&gt; boolean(), filter =&gt; <a href="#type-callback_filter">callback_filter()</a>, require =&gt; <a href="#type-callback_filter">callback_filter()</a>} | <a href="proplists.md#type-proplist">proplists:proplist()</a>
</code></pre>

 Shared options to control optic construction.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#chain-1">chain/1</a></td><td>
Combine existing optics into a chain.</td></tr><tr><td valign="top"><a href="#create-3">create/3</a></td><td>
Wrap an existing optic to cause it to create a new container when
the optic would otherwise return <code>{error, undefined}</code> or
<code>{error, required}</code> during a mapfold operation.</td></tr><tr><td valign="top"><a href="#error-1">error/1</a></td><td>
Always errors with the given reason.</td></tr><tr><td valign="top"><a href="#filter-1">filter/1</a></td><td>
Only focuses on the current data if the given filter function
returns true.</td></tr><tr><td valign="top"><a href="#fold-4">fold/4</a></td><td>
Given a list of optics, performs a recursive fold over the result
of focusing on the given data structure.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td>
Given a list of optics, returns a list of the values focused on by
the final optic.</td></tr><tr><td valign="top"><a href="#id-0">id/0</a></td><td>
Focus on what was given.</td></tr><tr><td valign="top"><a href="#is_optic-1">is_optic/1</a></td><td>
Check if a term is an optic.</td></tr><tr><td valign="top"><a href="#lax-1">lax/1</a></td><td>
Wrap an existing optic to cause it to skip an element when the
optic would otherwise return <code>{error, undefined}</code> or
<code>{error, required}</code> during a fold or mapfold operation.</td></tr><tr><td valign="top"><a href="#map-3">map/3</a></td><td>
Given a list of optics, performs a recursive map over the result of
focusing on the given data structure.</td></tr><tr><td valign="top"><a href="#mapfold-4">mapfold/4</a></td><td>
Given a list of optics, performs a recursive map and fold over the
result of focusing on the given data structure.</td></tr><tr><td valign="top"><a href="#merge-1">merge/1</a></td><td>
Merge existing optics into a single optic.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>
Create a new optic for traversing a data structure.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>
Create a new optic for traversing a data structure.</td></tr><tr><td valign="top"><a href="#put-3">put/3</a></td><td>
Given a list of optics, modifies the values focused on by
the final optic.</td></tr><tr><td valign="top"><a href="#require-1">require/1</a></td><td>
Only focuses on the current data if the given filter function
returns true.</td></tr><tr><td valign="top"><a href="#wrap-2">wrap/2</a></td><td>
Wrap an existing optic.</td></tr><tr><td valign="top"><a href="#wrap-3">wrap/3</a></td><td>
Wrap an existing optic.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="chain-1"></a>

### chain/1 ###

<pre><code>
chain(Optics) -&gt; <a href="#type-optic">optic()</a>
</code></pre>

<ul class="definitions"><li><code>Optics = <a href="#type-optics">optics()</a></code></li></ul>

`Optics`: The list of optics to compose.<br />

returns: An opaque optic record.

Combine existing optics into a chain. In left to right order, each
optic then focuses on the result of the previous optic. The result
of this composition is itself an optic.

This is the default composition method used for functions which
accept optics.

<a name="create-3"></a>

### create/3 ###

<pre><code>
create(Optic, New, Template) -&gt; <a href="#type-optic">optic()</a>
</code></pre>

<ul class="definitions"><li><code>Optic = <a href="#type-optic">optic()</a></code></li><li><code>New = <a href="#type-callback_new">callback_new()</a></code></li><li><code>Template = term()</code></li></ul>

`Optic`: The existing optic to wrap.<br />`New`: The callback function to apply when the mapfold fails.
  Must take two arguments, the existing data and a template argument
  to use to populate the new data. Should return the new container,
  which will immediately have the wrapped mapfold function re-applied
  after creation.<br />`Template`: The template value to be given to the callback function.<br />

returns: An opaque optic record.

Wrap an existing optic to cause it to create a new container when
the optic would otherwise return `{error, undefined}` or
`{error, required}` during a mapfold operation.

<a name="error-1"></a>

### error/1 ###

<pre><code>
error(Reason) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>

<ul class="definitions"><li><code>Reason = term()</code></li></ul>

`Reason`: The error description to return.<br />

returns: An opaque optic record.

Always errors with the given reason.

Example:

```
  > optic:get([optic:error(reason)], anything).
  {error, reason}
```

<a name="filter-1"></a>

### filter/1 ###

<pre><code>
filter(Filter) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>

<ul class="definitions"><li><code>Filter = <a href="#type-callback_filter">callback_filter()</a></code></li></ul>

`Filter`: The filter function to invoke to determine if the element should be
  focused. Takes the current data as an argument, returns a boolean
  true or false.<br />

returns: An opaque optic record.

Only focuses on the current data if the given filter function
returns true. Otherwise the data is skipped.

Can fail to be well behaved depending on if the filter criteria is
part of the focus.

Example:

```
  > IsOdd = fun (Elem) -> Elem % 2 == 1 end,
  > optic:get([optic:filter(IsOdd)], [1,2,3]).
  {ok, [1,3]}
```

<a name="fold-4"></a>

### fold/4 ###

<pre><code>
fold(Optics, Fold, Acc, Data) -&gt; <a href="#type-option">option</a>(NewAcc)
</code></pre>

<ul class="definitions"><li><code>Optics = <a href="#type-optics">optics()</a></code></li><li><code>Fold = <a href="#type-callback_fold">callback_fold()</a></code></li><li><code>Acc = term()</code></li><li><code>Data = term()</code></li><li><code>NewAcc = term()</code></li></ul>

`Optics`: A list of optics to apply. Leftmost is applied first.<br />`Fold`: The callback function to invoke on the focused elements and
  accumulator. Expected to return the modified accumulator.<br />`Acc`: The initial accumulator value.<br />`Data`: The container to apply the optics to.<br />

returns: 
On success, returns a tuple of ok and the final accumulator value.
On failure, returns an error tuple.

Given a list of optics, performs a recursive fold over the result
of focusing on the given data structure. The order of traversal is
determined by the optics used.

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(Optics, Data) -&gt; <a href="#type-option">option</a>(Values)
</code></pre>

<ul class="definitions"><li><code>Optics = <a href="#type-optics">optics()</a></code></li><li><code>Data = term()</code></li><li><code>Values = [term()]</code></li></ul>

`Optics`: A list of optics to apply. Leftmost is applied first.<br />`Data`: The container to apply the optics to.<br />

returns: A list of the focused values.

Given a list of optics, returns a list of the values focused on by
the final optic.

<a name="id-0"></a>

### id/0 ###

<pre><code>
id() -&gt; <a href="#type-optic">optic()</a>
</code></pre>
<br />

returns: An opaque optic record.

Focus on what was given.

This is the identity optic, it can be chained with any other optic
and will return the same optic.

Example:

```
  > optic:get([optic:id()], anything).
  {ok,[anything]}
```

<a name="is_optic-1"></a>

### is_optic/1 ###

<pre><code>
is_optic(Candidate) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>Candidate = term()</code></li></ul>

`Candidate`: The term to test.<br />

returns: A boolean flag.

Check if a term is an optic.

<a name="lax-1"></a>

### lax/1 ###

<pre><code>
lax(Optic) -&gt; <a href="#type-optic">optic()</a>
</code></pre>

<ul class="definitions"><li><code>Optic = <a href="#type-optic">optic()</a></code></li></ul>

`Optic`: The existing optic to wrap.<br />

returns: An opaque optic record.

Wrap an existing optic to cause it to skip an element when the
optic would otherwise return `{error, undefined}` or
`{error, required}` during a fold or mapfold operation.

<a name="map-3"></a>

### map/3 ###

<pre><code>
map(Optics, Map, Data) -&gt; <a href="#type-option">option</a>(NewData)
</code></pre>

<ul class="definitions"><li><code>Optics = <a href="#type-optics">optics()</a></code></li><li><code>Map = <a href="#type-callback_map">callback_map()</a></code></li><li><code>Data = term()</code></li><li><code>NewData = term()</code></li></ul>

`Optics`: A list of optics to apply. Leftmost is applied first.<br />`Map`: The callback function to invoke on the focused elements. Expected
  to return a modified element.<br />`Data`: The container to apply the optics to.<br />

returns: 
On success, returns a tuple of ok and the modified container.
On failure, returns an error tuple.

Given a list of optics, performs a recursive map over the result of
focusing on the given data structure.

<a name="mapfold-4"></a>

### mapfold/4 ###

<pre><code>
mapfold(Optics, MapFold, Acc, Data) -&gt; <a href="#type-option">option</a>({NewData, NewAcc})
</code></pre>

<ul class="definitions"><li><code>Optics = <a href="#type-optics">optics()</a></code></li><li><code>MapFold = <a href="#type-callback_mapfold">callback_mapfold()</a></code></li><li><code>Acc = term()</code></li><li><code>Data = term()</code></li><li><code>NewData = term()</code></li><li><code>NewAcc = term()</code></li></ul>

`Optics`: A list of optics to apply. Leftmost is applied first.<br />`MapFold`: The callback function to invoke on the focused elements and
  accumulator. Expected to return a tuple of the modified element and
  accumulator.<br />`Acc`: The initial accumulator value.<br />`Data`: The container to apply the optics to.<br />

returns: 
On success, returns a tuple of ok and a tuple of the modified
container and the final accumulator value. On failure, returns an
error tuple.

Given a list of optics, performs a recursive map and fold over the
result of focusing on the given data structure.

<a name="merge-1"></a>

### merge/1 ###

<pre><code>
merge(Optics) -&gt; <a href="#type-optic">optic()</a>
</code></pre>

<ul class="definitions"><li><code>Optics = <a href="#type-optics">optics()</a></code></li></ul>

`Optics`: The list of optics to compose.<br />

returns: An opaque optic record.

Merge existing optics into a single optic. In left to right order,
each optic focuses on the same data. The result of this composition
is itself an optic.

It is the optic product type.

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(MapFold) -&gt; <a href="#type-optic">optic()</a>
</code></pre>

<ul class="definitions"><li><code>MapFold = <a href="#type-optic_mapfold">optic_mapfold()</a></code></li></ul>

returns: An opaque optic record.

Create a new optic for traversing a data structure.

This is the less efficient form of optic construction and will
infer a fold function from the given mapfold function.

__See also:__ [new/2](#new-2).

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(MapFold, Fold) -&gt; <a href="#type-optic">optic()</a>
</code></pre>

<ul class="definitions"><li><code>MapFold = <a href="#type-optic_mapfold">optic_mapfold()</a></code></li><li><code>Fold = <a href="#type-optic_fold">optic_fold()</a></code></li></ul>

`MapFold`: At a minimum, an optic requires a mapfold function to be provided
  for both collecting and modifying values. This function must take
  three arguments; a callback function, an initial accumulator value
  and an arbitrary structure to traverse. The callback function will
  expect two values; an element and the current accumulator. It will
  return a two item tuple with the modified element and the modified
  accumulator. The mapfold function will return a two item tuple with
  the modified structure and the final accumulator value, wrapped in
  an ok or error tagged tuple.<br />`Fold`: A fold function can also be provided for more efficient traversal
  without modification. If one is not provided, it will be
  inefficiently inferred from the MapFold function. This function
  must take three arguments; a callback function, an initial
  accumulator value and an arbitrary structure to traverse. The
  callback function will expect two values; an element and the
  current accumulator. It will return the modified accumulator. The
  fold function will return a final accumulator value, wrapped in an
  ok or error tagged tuple.<br />

returns: An opaque optic record.

Create a new optic for traversing a data structure.

Well behaved optics should implement the following properties:

* Get -> Put: Writing the same value as was read should result in
the original structure.

* Put -> Get: Reading a value that was written should result in
the same value as was written.

* Put -> Put: Writing a value twice should result in only the
last written value.


Optics which are not well behaved will be more difficult to use and
compose with other optics. Their behaviour will change depending on
the order in which they are applied and the number of times they
are applied.

<a name="put-3"></a>

### put/3 ###

<pre><code>
put(Optics, Value, Data) -&gt; <a href="#type-option">option</a>(NewData)
</code></pre>

<ul class="definitions"><li><code>Optics = <a href="#type-optics">optics()</a></code></li><li><code>Value = term()</code></li><li><code>Data = term()</code></li><li><code>NewData = term()</code></li></ul>

`Optics`: A list of optics to apply. Leftmost is applied first.<br />`Data`: The container to apply the optics to.<br />

returns: 
On success, returns a tuple of ok and the modified container.
On failure, returns an error tuple.

Given a list of optics, modifies the values focused on by
the final optic.

<a name="require-1"></a>

### require/1 ###

<pre><code>
require(Filter) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>

<ul class="definitions"><li><code>Filter = <a href="#type-callback_filter">callback_filter()</a></code></li></ul>

`Filter`: The filter function to invoke to determine if the element should be
  focused. Takes the current data as an argument, returns a boolean
  true or false.<br />

returns: An opaque optic record.

Only focuses on the current data if the given filter function
returns true. Otherwise an `{error, required}` is returned.

Can fail to be well behaved depending on if the filter criteria is
part of the focus.

Example:

```
> IsOdd = fun (Elem) -> Elem % 2 == 1 end,
> optic:get([optic:require(IsOdd)], [1,2,3]).
{error, required}`''

<a name="wrap-2"></a>

### wrap/2 ###

<pre><code>
wrap(Optic, WrapMapFold) -&gt; <a href="#type-optic">optic()</a>
</code></pre>

<ul class="definitions"><li><code>Optic = <a href="#type-optic">optic()</a></code></li><li><code>WrapMapFold = <a href="#type-optic_wrap_mapfold">optic_wrap_mapfold()</a></code></li></ul>

returns: An opaque optic record.

Wrap an existing optic.

This is the less efficient form of optic construction and will
infer a fold wrapper from the given mapfold wrapper.

__See also:__ [wrap/3](#wrap-3).

<a name="wrap-3"></a>

### wrap/3 ###

<pre><code>
wrap(Optic, WrapMapFold, WrapFold) -&gt; <a href="#type-optic">optic()</a>
</code></pre>

<ul class="definitions"><li><code>Optic = <a href="#type-optic">optic()</a></code></li><li><code>WrapMapFold = <a href="#type-optic_wrap_mapfold">optic_wrap_mapfold()</a></code></li><li><code>WrapFold = <a href="#type-optic_wrap_fold">optic_wrap_fold()</a></code></li></ul>

`Optic`: An existing optic to modify.<br />`WrapMapFold`: A mapping function to apply to the optic's mapfold function.<br />`WrapFold`: A mapping function to apply to the optic's fold function.<br />

returns: An opaque optic record.

Wrap an existing optic.

This allows for modifying or replacing the methods of an existing
optic by applying a mapping function to each of the mapfold and
fold methods.

