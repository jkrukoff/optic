

# Module optic_generic #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

A set of generic optics that can be applied to multiple container
types.

<a name="description"></a>

## Description ##

Intended both as a convenience and to support optic creation from
parsed paths in optic_path.

Because of the ambiguous types they support, these optics do not
support the standard optic options. Instead, they always skip
unexpected types and never create missing values.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#index-1">index/1</a></td><td>
Focus on an element of a list like container.</td></tr><tr><td valign="top"><a href="#key-1">key/1</a></td><td>
Focus on the value of many different key/value like mappings.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="index-1"></a>

### index/1 ###

<pre><code>
index(Index::non_neg_integer()) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>
<br />

`Index`: The one based index of the element to focus on.<br />

returns: An opaque optic record.

Focus on an element of a list like container. Indexing begins at 1.
Understands how to focus on lists and tuples. Does not support the
usual optic options.

Example:

```
  > optic:get([optic_generic:index(3)], [1, 2, 3]).
  {ok,[3]}
```

<a name="key-1"></a>

### key/1 ###

<pre><code>
key(Key::term()) -&gt; <a href="optic.md#type-optic">optic:optic()</a>
</code></pre>
<br />

`Key`: The key to focus on.<br />

returns: An opaque optic record.

Focus on the value of many different key/value like mappings.
Understands how to focus on maps, property lists, dicts, orddicts
and gb_trees. Does not support the usual optic options.

Example:

```
  > optic:get([optic_generic:key(first)], #{first => 1}).
  {ok,[1]}
```

