# Day 5 - Part 2

<article class="day-desc"><h2 id="part2">--- Part Two ---</h2><p>Unfortunately, considering only horizontal and vertical lines doesn't give you the full picture; you need to also consider <strong>diagonal lines</strong>.</p>
<p>Because of the limits of the hydrothermal vent mapping system, the lines in your list will only ever be horizontal, vertical, or a diagonal line at exactly 45 degrees. In other words:</p>
<ul>
<li>An entry like <code>1,1 -&gt; 3,3</code> covers points <code>1,1</code>, <code>2,2</code>, and <code>3,3</code>.</li>
<li>An entry like <code>9,7 -&gt; 7,9</code> covers points <code>9,7</code>, <code>8,8</code>, and <code>7,9</code>.</li>
</ul>
<p>Considering all lines from the above example would now produce the following diagram:</p>
<pre><code>1.1....11.
.111...2..
..2.1.111.
...1.2.2..
.112313211
...1.2....
..1...1...
.1.....1..
1.......1.
222111....
</code></pre>
<p>You still need to determine <strong>the number of points where at least two lines overlap</strong>. In the above example, this is still anywhere in the diagram with a <code>2</code> or larger - now a total of <code><strong>12</strong></code> points.</p>
<p>Consider all of the lines. <strong>At how many points do at least two lines overlap?</strong></p>
</article>
