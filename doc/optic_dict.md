

# Module optic_dict #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

A set of optics specific to dicts.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all-0">all/0</a></td><td></td></tr><tr><td valign="top"><a href="#all-1">all/1</a></td><td></td></tr><tr><td valign="top"><a href="#association-1">association/1</a></td><td></td></tr><tr><td valign="top"><a href="#association-2">association/2</a></td><td>
Focus on the association for a dict key.</td></tr><tr><td valign="top"><a href="#associations-0">associations/0</a></td><td></td></tr><tr><td valign="top"><a href="#associations-1">associations/1</a></td><td>
Focus on all associations of a dict.</td></tr><tr><td valign="top"><a href="#key-1">key/1</a></td><td></td></tr><tr><td valign="top"><a href="#key-2">key/2</a></td><td>
Focus on the value of a dict key.</td></tr><tr><td valign="top"><a href="#keys-0">keys/0</a></td><td></td></tr><tr><td valign="top"><a href="#keys-1">keys/1</a></td><td>
Focus on all keys of a dict.</td></tr><tr><td valign="top"><a href="#values-0">values/0</a></td><td></td></tr><tr><td valign="top"><a href="#values-1">values/1</a></td><td>
Focus on all values of a dict.</td></tr></table>


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

<a name="association-1"></a>

### association/1 ###

<pre><code>
association(Key) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>

<ul class="definitions"><li><code>Key = term()</code></li></ul>

__See also:__ [association/2](#association-2).

<a name="association-2"></a>

### association/2 ###

<pre><code>
association(Key, Options) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>

<ul class="definitions"><li><code>Key = term()</code></li><li><code>Options = <a href="optic.md#type-variations">optic:variations()</a></code></li></ul>

`Key`: The key to focus on.<br />`Options`: Common optic options.<br />

returns: An opaque optic record.

Focus on the association for a dict key. An association is the
tuple of a dict key and value. If the key is modified, the optic is
no longer well behaved.

Example:

```
  > optic:get([optic_dict:association(first)],
              dict:from_list([{first, 1}, {second, 2}])).
  {ok,[{first,1}]}
```

<a name="associations-0"></a>

### associations/0 ###

<pre><code>
associations() -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>
<br />

__See also:__ [associations/1](#associations-1).

<a name="associations-1"></a>

### associations/1 ###

<pre><code>
associations(Options) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>

<ul class="definitions"><li><code>Options = <a href="optic.md#type-variations">optic:variations()</a></code></li></ul>

`Options`: Common optic options.<br />

returns: An opaque optic record.

Focus on all associations of a dict. An association is a tuple of
the key and value for each entry.

Example:

```
  > optic:get([optic_dict:associations()],
              dict:from_list([{first, 1}, {second, 2}])).
  {ok,[{first,1},{second,2}]}
```

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

Focus on the value of a dict key.

Example:

```
  > optic:get([optic_dict:key(first)],
              dict:from_list([{first, 1}, {second, 2}])).
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

Focus on all keys of a dict.

Example:

```
  > optic:get([optic_dict:keys()],
              dict:from_list([{first, 1}, {second, 2}])).
  {ok,[first,second]}
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

Focus on all values of a dict.

Example:

```
  > optic:get([optic_dict:values()],
              dict:from_list([{first, 1}, {second, 2}])).
  {ok,[1,2]}
```

