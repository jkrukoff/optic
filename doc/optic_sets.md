

# Module optic_sets #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

A set of optics specific to sets.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all-0">all/0</a></td><td></td></tr><tr><td valign="top"><a href="#all-1">all/1</a></td><td>
Focus on all elements of a set.</td></tr></table>


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

Focus on all elements of a set.

Example:

```
  > optic:get([optic_sets:all()], sets:from_list([1,2,3])).
  {ok,[1,2,3]}
```

