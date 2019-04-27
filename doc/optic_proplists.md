

# Module optic_proplists #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

A set of optics specific to proplists.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all-0">all/0</a></td><td></td></tr><tr><td valign="top"><a href="#all-1">all/1</a></td><td></td></tr><tr><td valign="top"><a href="#key-1">key/1</a></td><td></td></tr><tr><td valign="top"><a href="#key-2">key/2</a></td><td>
Focus on the value of a property list key.</td></tr><tr><td valign="top"><a href="#keys-0">keys/0</a></td><td></td></tr><tr><td valign="top"><a href="#keys-1">keys/1</a></td><td>
Focus on all keys of a list of properties.</td></tr><tr><td valign="top"><a href="#properties-0">properties/0</a></td><td></td></tr><tr><td valign="top"><a href="#properties-1">properties/1</a></td><td>
Focus on all properties of a list of properties.</td></tr><tr><td valign="top"><a href="#property-1">property/1</a></td><td></td></tr><tr><td valign="top"><a href="#property-2">property/2</a></td><td>
Focus on a property in a property list by key.</td></tr><tr><td valign="top"><a href="#values-0">values/0</a></td><td></td></tr><tr><td valign="top"><a href="#values-1">values/1</a></td><td>
Focus on all values of a list of properties.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all-0"></a>

### all/0 ###

<pre><code>
all() -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>
<br />

__See also:__ [values/1](#values-1).

<a name="all-1"></a>

### all/1 ###

<pre><code>
all(Options) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>

<ul class="definitions"><li><code>Options = <a href="optic.md#type-variations">optic:variations()</a></code></li></ul>

__See also:__ [values/1](#values-1).

<a name="key-1"></a>

### key/1 ###

<pre><code>
key(Key) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>

<ul class="definitions"><li><code>Key = term()</code></li></ul>

__See also:__ [key/2](#key-2).

<a name="key-2"></a>

### key/2 ###

<pre><code>
key(Key, Options) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>

<ul class="definitions"><li><code>Key = term()</code></li><li><code>Options = <a href="optic.md#type-variations">optic:variations()</a></code></li></ul>

`Key`: The key to focus on.<br />`Options`: Common optic options.<br />

returns: An opaque optic record.

Focus on the value of a property list key. As keys may be
duplicated, this may be multiple values. If the value is not given,
it defaults to the atom `true`.

Example:

```
  > optic:get([optic_proplists:key(first)], [{first, 1}, {second, 2}]).
  {ok,[1]}
```

<a name="keys-0"></a>

### keys/0 ###

<pre><code>
keys() -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>
<br />

__See also:__ [keys/1](#keys-1).

<a name="keys-1"></a>

### keys/1 ###

<pre><code>
keys(Options) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>

<ul class="definitions"><li><code>Options = <a href="optic.md#type-variations">optic:variations()</a></code></li></ul>

`Options`: Common optic options.<br />

returns: An opaque optic record.

Focus on all keys of a list of properties. Duplicate keys are
preserved.

Example:

```
  > optic:get([optic_proplists:keys()], [{first, 1}, {second, 2}]).
  {ok,[first,second]}
```

<a name="properties-0"></a>

### properties/0 ###

<pre><code>
properties() -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>
<br />

__See also:__ [properties/1](#properties-1).

<a name="properties-1"></a>

### properties/1 ###

<pre><code>
properties(Options) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>

<ul class="definitions"><li><code>Options = <a href="optic.md#type-variations">optic:variations()</a></code></li></ul>

`Options`: Common optic options.<br />

returns: An opaque optic record.

Focus on all properties of a list of properties. A propety is a
tuple of a key and value. If a value was not given, it defaults to
the atom `true`.

Example:

```
  > optic:get([optic_proplists:properties()], [{first, 1}, {second, 2}]).
  {ok,[{first,1},{second,2}]}
```

<a name="property-1"></a>

### property/1 ###

<pre><code>
property(Key) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>

<ul class="definitions"><li><code>Key = term()</code></li></ul>

__See also:__ [property/2](#property-2).

<a name="property-2"></a>

### property/2 ###

<pre><code>
property(Key, Options) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>

<ul class="definitions"><li><code>Key = term()</code></li><li><code>Options = <a href="optic.md#type-variations">optic:variations()</a></code></li></ul>

`Key`: The key to focus on.<br />`Options`: Common optic options.<br />

returns: An opaque optic record.

Focus on a property in a property list by key. As keys may be
duplicated, this may be multiple properties. If the value is not
given, it defaults to the atom `true`. If the key is modified, the
optic is no longer well behaved.

Example:

```
  > optic:get([optic_proplists:property(first)], [{first, 1}, {second, 2}]).
  {ok,[{first,1}]}
```

<a name="values-0"></a>

### values/0 ###

<pre><code>
values() -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>
<br />

__See also:__ [values/1](#values-1).

<a name="values-1"></a>

### values/1 ###

<pre><code>
values(Options) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>

<ul class="definitions"><li><code>Options = <a href="optic.md#type-variations">optic:variations()</a></code></li></ul>

`Options`: Common optic options.<br />

returns: An opaque optic record.

Focus on all values of a list of properties.

Example:

```
  > optic:get([optic_proplists:values()], [{first, 1}, {second, 2}]).
  {ok,[1,2]}
```

