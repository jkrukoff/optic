

# Module optic_lists #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

A set of optics specific to lists.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all-0">all/0</a></td><td></td></tr><tr><td valign="top"><a href="#all-1">all/1</a></td><td>
Focus on all elements of a list.</td></tr><tr><td valign="top"><a href="#head-0">head/0</a></td><td></td></tr><tr><td valign="top"><a href="#head-1">head/1</a></td><td>
Focus on the head of a list.</td></tr><tr><td valign="top"><a href="#nth-1">nth/1</a></td><td></td></tr><tr><td valign="top"><a href="#nth-2">nth/2</a></td><td>
Focus on the nth element of a list.</td></tr><tr><td valign="top"><a href="#tail-0">tail/0</a></td><td></td></tr><tr><td valign="top"><a href="#tail-1">tail/1</a></td><td>
Focus on the tail of a list.</td></tr></table>


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

Focus on all elements of a list.

Example:

```
  > optic:get([optic_lists:all()], [1,2,3]).
  {ok,[1,2,3]}
```

<a name="head-0"></a>

### head/0 ###

<pre><code>
head() -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>
<br />

__See also:__ [head/1](#head-1).

<a name="head-1"></a>

### head/1 ###

<pre><code>
head(Options::<a href="optic.md#type-extend_options">optic:extend_options()</a>) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>
<br />

`Options`: Common optic options.<br />

returns: An opaque optic record.

Focus on the head of a list. The list must have at least one
element to have a head.

Example:

```
  > optic:get([optic_lists:head()], [1,2,3]).
  {ok,[1]}
```

<a name="nth-1"></a>

### nth/1 ###

<pre><code>
nth(N::pos_integer()) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>
<br />

__See also:__ [nth/2](#nth-2).

<a name="nth-2"></a>

### nth/2 ###

<pre><code>
nth(N::pos_integer(), Options::<a href="optic.md#type-extend_options">optic:extend_options()</a>) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>
<br />

`N`: The index of the list element to focus on.<br />`Options`: Common optic options.<br />

returns: An opaque optic record.

Focus on the nth element of a list. As with `lists:nth/2`, indexing
begins at 1.

Example:

```
  > optic:get([optic_lists:nth(1)], [1,2,3]).
  {ok,[1]}
```

<a name="tail-0"></a>

### tail/0 ###

<pre><code>
tail() -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>
<br />

__See also:__ [tail/1](#tail-1).

<a name="tail-1"></a>

### tail/1 ###

<pre><code>
tail(Options::<a href="optic.md#type-extend_options">optic:extend_options()</a>) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>
<br />

`Options`: Common optic options.<br />

returns: An opaque optic record.

Focus on the tail of a list. A list must have at least one element
to have a tail.

Example:

```
  > optic:get([optic_lists:tail()], [1,2,3]).
  {ok,[2,3]}
```

