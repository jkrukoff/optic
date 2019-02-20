

# Module optic_path #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Utility functions for constructing optics from lists of selectors.

<a name="types"></a>

## Data Types ##




### <a name="type-path">path()</a> ###


<pre><code>
path() = string() | binary() | non_neg_integer() | *
</code></pre>

 A single path component.



### <a name="type-paths">paths()</a> ###


<pre><code>
paths() = [<a href="#type-path">path()</a>]
</code></pre>

 A list of path components.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-1">new/1</a></td><td>
Construct a list of optics from a path.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Paths::<a href="#type-paths">paths()</a>) -&gt; <a href="optic.md#type-optics">optic:optics()</a>
</code></pre>
<br />

`Paths`: A list of path components to convert.<br />

returns: A list of opaque optic records.

Construct a list of optics from a path. The type of the path
component determines the optic used:

* string: A key for a map-like structure.

* binary: A key for a map-like structure.

* integer: An index into a list-like structure.

* '*': All elements of a list.


This heavily depends on the `optic_generic` module, see the optics
there for the full list of containers supported.

Example:

```
  > optic:get(optic_path(["first"]), ${"first" => 1, "second" => 2}).
  {ok,[1]}
```

