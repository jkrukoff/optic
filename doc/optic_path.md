

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




### <a name="type-paths">paths()</a> ###


<pre><code>
paths() = [<a href="#type-path">path()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Paths::<a href="#type-paths">paths()</a>) -&gt; <a href="optic.md#type-optics">optic:optics()</a>
</code></pre>
<br />

