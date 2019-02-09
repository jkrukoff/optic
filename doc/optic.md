

# Module optic #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

.

<a name="types"></a>

## Data Types ##




### <a name="type-callback_fold">callback_fold()</a> ###


<pre><code>
callback_fold() = fun((term(), term()) -&gt; term())
</code></pre>




### <a name="type-callback_map">callback_map()</a> ###


<pre><code>
callback_map() = fun((term()) -&gt; term())
</code></pre>




### <a name="type-callback_mapfold">callback_mapfold()</a> ###


<pre><code>
callback_mapfold() = fun((term(), term()) -&gt; {term(), term()})
</code></pre>




### <a name="type-error">error()</a> ###


<pre><code>
error() = {error, atom()}
</code></pre>




### <a name="type-extend_options">extend_options()</a> ###


<pre><code>
extend_options() = #{create =&gt; term(), strict =&gt; boolean()} | [<a href="proplists.md#type-property">proplists:property()</a>]
</code></pre>




### <a name="type-optic">optic()</a> ###


__abstract datatype__: `optic()`




### <a name="type-optic_fold">optic_fold()</a> ###


<pre><code>
optic_fold() = fun((<a href="#type-callback_fold">callback_fold()</a>, term(), term()) -&gt; <a href="#type-option">option</a>(term()))
</code></pre>




### <a name="type-optic_mapfold">optic_mapfold()</a> ###


<pre><code>
optic_mapfold() = fun((<a href="#type-callback_mapfold">callback_mapfold()</a>, term(), term()) -&gt; <a href="#type-option">option</a>({term(), term()}))
</code></pre>




### <a name="type-optics">optics()</a> ###


__abstract datatype__: `optics()`




### <a name="type-option">option()</a> ###


<pre><code>
option(Success) = {ok, Success} | <a href="#type-error">error()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#fold-4">fold/4</a></td><td>
Given a list of optics, performs a recursive fold over the result
of focusing on the given data structure.</td></tr><tr><td valign="top"><a href="#from-1">from/1</a></td><td>
Compose a list of optics into a single optic which traverses over
the same structure.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td>
Given a list of optics, returns a list of the values focused on by
the final optic.</td></tr><tr><td valign="top"><a href="#map-3">map/3</a></td><td>
Given a list of optics, performs a recursive map over teh result of
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
fold(Optics::<a href="#type-optics">optics()</a>, Data::term(), Fold::<a href="#type-callback_fold">callback_fold()</a>, Acc::term()) -&gt; <a href="#type-option">option</a>(term())
</code></pre>
<br />

Given a list of optics, performs a recursive fold over the result
of focusing on the given data structure.

<a name="from-1"></a>

### from/1 ###

<pre><code>
from(Tail::<a href="#type-optics">optics()</a>) -&gt; <a href="#type-optic">optic()</a>
</code></pre>
<br />

Compose a list of optics into a single optic which traverses over
the same structure. This is used to combine multiple optics into a
single optic which traverses over the sum of all the targets.

This is different than the composition used by the traversal
functions which apply the composed optics to the previous focused
value, instead of to the same value.

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(Optics::<a href="#type-optics">optics()</a>, Data::term()) -&gt; <a href="#type-option">option</a>([term()])
</code></pre>
<br />

Given a list of optics, returns a list of the values focused on by
the final optic.

<a name="map-3"></a>

### map/3 ###

<pre><code>
map(Optics::<a href="#type-optics">optics()</a>, Data::term(), Map::<a href="#type-callback_map">callback_map()</a>) -&gt; <a href="#type-option">option</a>(term())
</code></pre>
<br />

Given a list of optics, performs a recursive map over teh result of
focusing on the given data structure.

<a name="mapfold-4"></a>

### mapfold/4 ###

<pre><code>
mapfold(Optics::<a href="#type-optics">optics()</a>, Data::term(), MapFold::<a href="#type-callback_mapfold">callback_mapfold()</a>, Acc::term()) -&gt; <a href="#type-option">option</a>({term(), term()})
</code></pre>
<br />

Given a list of optics, performs a recursive map and fold over the
result of focusing on the given data structure.

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(MapFold::<a href="#type-optic_mapfold">optic_mapfold()</a> | undefined) -&gt; <a href="#type-optic">optic()</a>
</code></pre>
<br />

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

Create a new optic for traversing a data structure.

At a minimum, an optic requires a mapfold function to be provided
for both collecting and modifying values. This function must take
three arguments; a callback function, an initial accumulator value
and an arbitrary structure to traverse. The callback function will
expect two values; an element and the current accumulator. It will
return a two item tuple with the modified element and the modified
accumulator. The mapfold function will return a two item tuple with
the modified structure and the final accumulator value, wrapped in
an ok or error tagged tuple.

A fold function can also be provided for more efficient traversal
without modification. If one is not provided, it will be
inefficiently inferred from the mapfold function. This function
must take three arguments; a callback function, an initial
accumulator value and an arbitrary structure to traverse. The
callback function will expect two values; an element and the
current accumulator. It will return the modified accumulator. The
fold function will return a final accumulator value, wrapped in an
ok or error tagged tuple.

To compose optics without unexpected side effects, the following
properties should hold:

* Get -> Put: Writing the same value as was read should result in
the original structure.

* Put -> Get: Reading a value that was written should result in
the same value as was written.

* Put -> Put: Writing a value twice should result in only the
last written value.


<a name="put-3"></a>

### put/3 ###

<pre><code>
put(Optics::<a href="#type-optics">optics()</a>, Data::term(), Value::term()) -&gt; <a href="#type-option">option</a>(term())
</code></pre>
<br />

Given a list of optics, modifies the values focused on by
the final optic.

