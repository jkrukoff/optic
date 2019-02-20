

# Module optic_tuples #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

A set of optics specific to tuples.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all-0">all/0</a></td><td></td></tr><tr><td valign="top"><a href="#all-1">all/1</a></td><td>
Focus on all elements of a tuple.</td></tr><tr><td valign="top"><a href="#element-1">element/1</a></td><td></td></tr><tr><td valign="top"><a href="#element-2">element/2</a></td><td>
Focus on the nth element of a tuple.</td></tr><tr><td valign="top"><a href="#field-3">field/3</a></td><td></td></tr><tr><td valign="top"><a href="#field-4">field/4</a></td><td>
Focus on a record field.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all-0"></a>

### all/0 ###

<pre><code>
all() -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>
<br />

__See also:__ [all/1](#all-1).

<a name="all-1"></a>

### all/1 ###

<pre><code>
all(Options::<a href="optic.md#type-extend_options">optic:extend_options()</a>) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>
<br />

`Options`: Common optic options.<br />

returns: An opaque optic record.

Focus on all elements of a tuple.

Example:

```
  > optic:get([optic_tuples:all()], {1,2,3}).
  {ok,[1,2,3]}
```

<a name="element-1"></a>

### element/1 ###

<pre><code>
element(N::pos_integer()) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>
<br />

__See also:__ [element/2](#element-2).

<a name="element-2"></a>

### element/2 ###

<pre><code>
element(N::pos_integer(), Options::<a href="optic.md#type-extend_options">optic:extend_options()</a>) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>
<br />

`N`: The index of the tuple element to focus on.<br />`Options`: Common optic options.<br />

returns: An opaque optic record.

Focus on the nth element of a tuple. As with `erlang:element/2`,
indexing begins at 1.

Example:

```
  > optic:get([optic_tuples:element(1)], {1,2,3}).
  {ok,[1]}
```

<a name="field-3"></a>

### field/3 ###

<pre><code>
field(Tag::atom(), Size::pos_integer(), N::pos_integer()) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>
<br />

__See also:__ [field/4](#field-4).

<a name="field-4"></a>

### field/4 ###

<pre><code>
field(Tag::atom(), Size::pos_integer(), N::pos_integer(), Options::<a href="optic.md#type-extend_options">optic:extend_options()</a>) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>
<br />

`Tag`: The expected record tag.<br />`Size`: The expected record size.<br />`N`: The index of the field in the record tuple.<br />`Options`: Common optic options.<br />

returns: An opaque optic record.

Focus on a record field. As records are a compiler construct, this
depends on the `?OPTIC_FIELD` macro in `include/optic_tuples.hrl`
to construct the required arguments from the record definition.

Given the record definition:

```
  -include_lib("optic/include/optic_tuples.hrl").
  -record(example, {first}).
```

Example:

```
  > optic:get([optic_tuples:field(?OPTIC_FIELD(example, first))],
              #example{first=1}).
  {ok,[1]}
```

