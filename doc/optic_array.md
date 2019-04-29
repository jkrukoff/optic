

# Module optic_array #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

A set of optics specific to arrays.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all-0">all/0</a></td><td></td></tr><tr><td valign="top"><a href="#all-1">all/1</a></td><td>
Focus on all values of an array.</td></tr><tr><td valign="top"><a href="#nth-1">nth/1</a></td><td></td></tr><tr><td valign="top"><a href="#nth-2">nth/2</a></td><td>
Focus on the nth value of an array.</td></tr></table>


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
all(Options) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>

<ul class="definitions"><li><code>Options = <a href="optic.md#type-variations">optic:variations()</a></code></li></ul>

`Options`: Common optic options.<br />

returns: An opaque optic record.

Focus on all values of an array.

Example:

```
  > optic:get([optic_array:all()], array:from_list([1,2,3])).
  {ok,[1,2,3]}
```

<a name="nth-1"></a>

### nth/1 ###

<pre><code>
nth(N) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>

<ul class="definitions"><li><code>N = pos_integer()</code></li></ul>

__See also:__ [nth/2](#nth-2).

<a name="nth-2"></a>

### nth/2 ###

<pre><code>
nth(N, Options) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>

<ul class="definitions"><li><code>N = pos_integer()</code></li><li><code>Options = <a href="optic.md#type-variations">optic:variations()</a></code></li></ul>

`N`: The index of the array value to focus on.<br />`Options`: Common optic options.<br />

returns: An opaque optic record.

Focus on the nth value of an array. Like lists, but unlike the
standard array operations, indexing begins at 1.

Example:

```
  > optic:get([optic_array:nth(1)], array:from_list([1,2,3])).
  {ok,[1]}
```

