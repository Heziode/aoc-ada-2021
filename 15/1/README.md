# Day 15 - Part 1

<article class="day-desc"><p>You've almost reached the exit of the cave, but the walls are getting closer together. Your submarine can barely still fit, though; the main problem is that the walls of the cave are covered in <a href="https://en.wikipedia.org/wiki/Chiton" target="_blank">chitons</a>, and it would be best not to bump any of them.</p>
<p>The cavern is large, but has a very low ceiling, restricting your motion to two dimensions. The shape of the cavern resembles a square; a quick scan of chiton density produces a map of <strong>risk level</strong> throughout the cave (your puzzle input). For example:</p>
<pre><code>1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
</code></pre>
<p>You start in the top left position, your destination is the bottom right position, and you <span title="Can't go diagonal until we can repair the caterpillar unit. Could be the liquid helium or the superconductors.">cannot move diagonally</span>. The number at each position is its <strong>risk level</strong>; to determine the total risk of an entire path, add up the risk levels of each position you <strong>enter</strong> (that is, don't count the risk level of your starting position unless you enter it; leaving it adds no risk to your total).</p>
<p>Your goal is to find a path with the <strong>lowest total risk</strong>. In this example, a path with the lowest total risk is highlighted here:</p>
<pre><code><strong>1</strong>163751742
<strong>1</strong>381373672
<strong>2136511</strong>328
369493<strong>15</strong>69
7463417<strong>1</strong>11
1319128<strong>13</strong>7
13599124<strong>2</strong>1
31254216<strong>3</strong>9
12931385<strong>21</strong>
231194458<strong>1</strong>
</code></pre>
<p>The total risk of this path is <code><strong>40</strong></code> (the starting position is never entered, so its risk is not counted).</p>
<p><strong>What is the lowest total risk of any path from the top left to the bottom right?</strong></p>
</article>
