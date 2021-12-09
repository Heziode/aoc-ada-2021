# Day 9 - Part 2

<article class="day-desc"><p>Next, you need to find the largest basins so you know what areas are most important to avoid.</p>
<p>A <strong>basin</strong> is all locations that eventually flow downward to a single low point. Therefore, every low point has a basin, although some basins are very small. Locations of height <code>9</code> do not count as being in any basin, and all other locations will always be part of exactly one basin.</p>
<p>The <strong>size</strong> of a basin is the number of locations within the basin, including the low point. The example above has four basins.</p>
<p>The top-left basin, size <code>3</code>:</p>
<pre><code><strong>21</strong>99943210
<strong>3</strong>987894921
9856789892
8767896789
9899965678
</code></pre>
<p>The top-right basin, size <code>9</code>:</p>
<pre><code>21999<strong>43210</strong>
398789<strong>4</strong>9<strong>21</strong>
985678989<strong>2</strong>
8767896789
9899965678
</code></pre>
<p>The middle basin, size <code>14</code>:</p>
<pre><code>2199943210
39<strong>878</strong>94921
9<strong>85678</strong>9892
<strong>87678</strong>96789
9<strong>8</strong>99965678
</code></pre>
<p>The bottom-right basin, size <code>9</code>:</p>
<pre><code>2199943210
3987894921
9856789<strong>8</strong>92
876789<strong>678</strong>9
98999<strong>65678</strong>
</code></pre>
<p>Find the three largest basins and multiply their sizes together. In the above example, this is <code>9 * 14 * 9 = <strong>1134</strong></code>.</p>
<p><strong>What do you get if you multiply together the sizes of the three largest basins?</strong></p>
</article>