# Day 9 - Part 2

<article class="day-desc"><h2 id="part2">--- Part Two ---</h2><p>Next, you need to find the largest basins so you know what areas are most important to avoid.</p>
<p>A <em>basin</em> is all locations that eventually flow downward to a single low point. Therefore, every low point has a basin, although some basins are very small. Locations of height <code>9</code> do not count as being in any basin, and all other locations will always be part of exactly one basin.</p>
<p>The <em>size</em> of a basin is the number of locations within the basin, including the low point. The example above has four basins.</p>
<p>The top-left basin, size <code>3</code>:</p>
<pre><code><em>21</em>99943210
<em>3</em>987894921
9856789892
8767896789
9899965678
</code></pre>
<p>The top-right basin, size <code>9</code>:</p>
<pre><code>21999<em>43210</em>
398789<em>4</em>9<em>21</em>
985678989<em>2</em>
8767896789
9899965678
</code></pre>
<p>The middle basin, size <code>14</code>:</p>
<pre><code>2199943210
39<em>878</em>94921
9<em>85678</em>9892
<em>87678</em>96789
9<em>8</em>99965678
</code></pre>
<p>The bottom-right basin, size <code>9</code>:</p>
<pre><code>2199943210
3987894921
9856789<em>8</em>92
876789<em>678</em>9
98999<em>65678</em>
</code></pre>
<p>Find the three largest basins and multiply their sizes together. In the above example, this is <code>9 * 14 * 9 = <em>1134</em></code>.</p>
<p><em>What do you get if you multiply together the sizes of the three largest basins?</em></p>
</article>