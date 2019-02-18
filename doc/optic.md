

# Module optic #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

A library for creating "optics", a composable traversal over
arbitrary containers.

<a name="description"></a>

## Description ##
These optics can then be composed to read and update nested data
structures.
<a name="types"></a>

## Data Types ##




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



### <a name="type-error">error()</a> ###


<pre><code>
error() = {error, term()}
</code></pre>

 The expected error format.



### <a name="type-extend_options">extend_options()</a> ###


<pre><code>
extend_options() = #{create =&gt; term(), strict =&gt; boolean()} | [<a href="proplists.md#type-property">proplists:property()</a>]
</code></pre>

 Shared options to control optic construction.



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

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#fold-4">fold/4</a></td><td>
Given a list of optics, performs a recursive fold over the result
of focusing on the given data structure.</td></tr><tr><td valign="top"><a href="#from-1">from/1</a></td><td>
Compose a list of optics into a single optic which traverse over
the same structure.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td>
Given a list of optics, returns a list of the values focused on by
the final optic.</td></tr><tr><td valign="top"><a href="#map-3">map/3</a></td><td>
Given a list of optics, performs a recursive map over the result of
focusing on the given data structure.</td></tr><tr><td valign="top"><a href="#mapfold-4">mapfold/4</a></td><td>
Given a list of optics, performs a recursive map and fold over the
result of focusing on the given data structure.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>
Create a new optic for traversing a data structure.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>
Create a new optic for traversing a data structure.</td></tr><tr><td valign="top"><a href="#put-3">put/3</a></td><td>
Given a list of optics, modifies the values focused on by
the final optic.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="fold-4"></a>

### fold/4 ###

<pre><code>
fold(Optics::<a href="#type-optics">optics()</a>, Data::term(), Fold::<a href="#type-callback_fold">callback_fold()</a>, Acc::term()) -&gt; NewAcc::<a href="#type-option">option</a>(term())
</code></pre>
<br />

`Optics`: A list of optics to apply. Leftmost is applied first.<br />`Data`: The container to apply the optics to.<br />`Fold`: The callback function to invoke on the focused elements and
  accumulator. Expected to return the modified accumulator.<br />`Acc`: The initial accumulator value.<br />

returns: 
On success, returns a tuple of ok and the final accumulator value.
On failure, returns an error tuple.

Given a list of optics, performs a recursive fold over the result
of focusing on the given data structure. The order of traversal is
determined by the optics used.

<a name="from-1"></a>

### from/1 ###

<pre><code>
from(Optics::<a href="#type-optics">optics()</a>) -&gt; <a href="#type-optic">optic()</a>
</code></pre>
<br />

`Optics`: A list of optics to compose. Leftmost is applied first.<br />

returns: An opaque optic record.

Compose a list of optics into a single optic which traverse over
the same structure. This is used to combine multiple optics into a
single optic which traverses over the sum of all the targets.

This is different than the composition used by the traversal
functions which apply the composed optics to the previous focused
value, instead of to the same value.

__See also:__ [new/2](#new-2).

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(Optics::<a href="#type-optics">optics()</a>, Data::term()) -&gt; <a href="#type-option">option</a>([term()])
</code></pre>
<br />

`Optics`: A list of optics to apply. Leftmost is applied first.<br />`Data`: The container to apply the optics to.<br />

returns: A list of the focused values.

Given a list of optics, returns a list of the values focused on by
the final optic.

<a name="map-3"></a>

### map/3 ###

<pre><code>
map(Optics::<a href="#type-optics">optics()</a>, Data::term(), Map::<a href="#type-callback_map">callback_map()</a>) -&gt; <a href="#type-option">option</a>(NewData::term())
</code></pre>
<br />

`Optics`: A list of optics to apply. Leftmost is applied first.<br />`Data`: The container to apply the optics to.<br />`Map`: The callback function to invoke on the focused elements. Expected
  to return a modified element.<br />

returns: 
On success, returns a tuple of ok and the modified container.
On failure, returns an error tuple.

Given a list of optics, performs a recursive map over the result of
focusing on the given data structure.

<a name="mapfold-4"></a>

### mapfold/4 ###

<pre><code>
mapfold(Optics::<a href="#type-optics">optics()</a>, Data::term(), MapFold::<a href="#type-callback_mapfold">callback_mapfold()</a>, Acc::term()) -&gt; <a href="#type-option">option</a>({NewData::term(), NewAcc::term()})
</code></pre>
<br />

`Optics`: A list of optics to apply. Leftmost is applied first.<br />`Data`: The container to apply the optics to.<br />`MapFold`: The callback function to invoke on the focused elements and
  accumulator. Expected to return a tuple of the modified element and
  accumulator.<br />`Acc`: The initial accumulator value.<br />

returns: 
On success, returns a tuple of ok and a tuple of the modified
container and the final accumulator value. On failure, returns an
error tuple.

Given a list of optics, performs a recursive map and fold over the
result of focusing on the given data structure.

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(MapFold::<a href="#type-optic_mapfold">optic_mapfold()</a> | undefined) -&gt; <a href="#type-optic">optic()</a>
</code></pre>
<br />

returns: An opaque optic record.

Create a new optic for traversing a data structure.

This is the less efficient form of optic construction and will
infer a fold function from the given mapfold function.

__See also:__ [new/2](#new-2).

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(MapFold::<a href="#type-optic_mapfold">optic_mapfold()</a> | undefined, Fold::<a href="#type-optic_fold">optic_fold()</a> | undefined) -&gt; <a href="#type-optic">optic()</a>
</code></pre>
<br />

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
put(Optics::<a href="#type-optics">optics()</a>, Data::term(), Value::term()) -&gt; <a href="#type-option">option</a>(term())
</code></pre>
<br />

`Optics`: A list of optics to apply. Leftmost is applied first.<br />`Data`: The container to apply the optics to.<br />

returns: 
On success, returns a tuple of ok and the modified container.
On failure, returns an error tuple.

Given a list of optics, modifies the values focused on by
the final optic.

