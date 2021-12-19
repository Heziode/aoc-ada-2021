# Day 18 - Part 1

<article class="day-desc"><p>You descend into the ocean trench and encounter some <a href="https://en.wikipedia.org/wiki/Snailfish" target="_blank">snailfish</a>. They say they saw the sleigh keys! They'll even tell you which direction the keys went if you help one of the smaller snailfish with his <strong><span title="Snailfish math is snailfish math!">math</span> homework</strong>.</p>
<p>Snailfish numbers aren't like regular numbers. Instead, every snailfish number is a <strong>pair</strong> - an ordered list of two elements. Each element of the pair can be either a regular number or another pair.</p>
<p>Pairs are written as <code>[x,y]</code>, where <code>x</code> and <code>y</code> are the elements within the pair. Here are some example snailfish numbers, one snailfish number per line:</p>
<pre><code>[1,2]
[[1,2],3]
[9,[8,7]]
[[1,9],[8,5]]
[[[[1,2],[3,4]],[[5,6],[7,8]]],9]
[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]
</code></pre>
<p>This snailfish homework is about <strong>addition</strong>. To add two snailfish numbers, form a pair from the left and right parameters of the addition operator. For example, <code>[1,2]</code> + <code>[[3,4],5]</code> becomes <code>[[1,2],[[3,4],5]]</code>.</p>
<p>There's only one problem: <strong>snailfish numbers must always be reduced</strong>, and the process of adding two snailfish numbers can result in snailfish numbers that need to be reduced.</p>
<p>To <strong>reduce a snailfish number</strong>, you must repeatedly do the first action in this list that applies to the snailfish number:</p>
<ul>
<li>If any pair is <strong>nested inside four pairs</strong>, the leftmost such pair <strong>explodes</strong>.</li>
<li>If any regular number is <strong>10 or greater</strong>, the leftmost such regular number <strong>splits</strong>.</li>
</ul>
<p>Once no action in the above list applies, the snailfish number is reduced.</p>
<p>During reduction, at most one action applies, after which the process returns to the top of the list of actions. For example, if <strong>split</strong> produces a pair that meets the <strong>explode</strong> criteria, that pair <strong>explodes</strong> before other <strong>splits</strong> occur.</p>
<p>To <strong>explode</strong> a pair, the pair's left value is added to the first regular number to the left of the exploding pair (if any), and the pair's right value is added to the first regular number to the right of the exploding pair (if any). Exploding pairs will always consist of two regular numbers. Then, the entire exploding pair is replaced with the regular number <code>0</code>.</p>
<p>Here are some examples of a single explode action:</p>
<ul>
<li><code>[[[[<strong>[9,8]</strong>,1],2],3],4]</code> becomes <code>[[[[<strong>0</strong>,<strong>9</strong>],2],3],4]</code> (the <code>9</code> has no regular number to its left, so it is not added to any regular number).</li>
<li><code>[7,[6,[5,[4,<strong>[3,2]</strong>]]]]</code> becomes <code>[7,[6,[5,[<strong>7</strong>,<strong>0</strong>]]]]</code> (the <code>2</code> has no regular number to its right, and so it is not added to any regular number).</li>
<li><code>[[6,[5,[4,<strong>[3,2]</strong>]]],1]</code> becomes <code>[[6,[5,[<strong>7</strong>,<strong>0</strong>]]],<strong>3</strong>]</code>.</li>
<li><code>[[3,[2,[1,<strong>[7,3]</strong>]]],[6,[5,[4,[3,2]]]]]</code> becomes <code>[[3,[2,[<strong>8</strong>,<strong>0</strong>]]],[<strong>9</strong>,[5,[4,[3,2]]]]]</code> (the pair <code>[3,2]</code> is unaffected because the pair <code>[7,3]</code> is further to the left; <code>[3,2]</code> would explode on the next action).</li>
<li><code>[[3,[2,[8,0]]],[9,[5,[4,<strong>[3,2]</strong>]]]]</code> becomes <code>[[3,[2,[8,0]]],[9,[5,[<strong>7</strong>,<strong>0</strong>]]]]</code>.</li>
</ul>
<p>To <strong>split</strong> a regular number, replace it with a pair; the left element of the pair should be the regular number divided by two and rounded <strong>down</strong>, while the right element of the pair should be the regular number divided by two and rounded <strong>up</strong>. For example, <code>10</code> becomes <code>[5,5]</code>, <code>11</code> becomes <code>[5,6]</code>, <code>12</code> becomes <code>[6,6]</code>, and so on.</p>
<p>Here is the process of finding the reduced result of <code>[[[[4,3],4],4],[7,[[8,4],9]]]</code> + <code>[1,1]</code>:</p>
<pre><code>after addition: [[[[<strong>[4,3]</strong>,4],4],[7,[[8,4],9]]],[1,1]]
after explode:  [[[[0,7],4],[7,[<strong>[8,4]</strong>,9]]],[1,1]]
after explode:  [[[[0,7],4],[<strong>15</strong>,[0,13]]],[1,1]]
after split:    [[[[0,7],4],[[7,8],[0,<strong>13</strong>]]],[1,1]]
after split:    [[[[0,7],4],[[7,8],[0,<strong>[6,7]</strong>]]],[1,1]]
after explode:  [[[[0,7],4],[[7,8],[6,0]]],[8,1]]
</code></pre>
<p>Once no reduce actions apply, the snailfish number that remains is the actual result of the addition operation: <code>[[[[0,7],4],[[7,8],[6,0]]],[8,1]]</code>.</p>
<p>The homework assignment involves adding up a <strong>list of snailfish numbers</strong> (your puzzle input). The snailfish numbers are each listed on a separate line. Add the first snailfish number and the second, then add that result and the third, then add that result and the fourth, and so on until all numbers in the list have been used once.</p>
<p>For example, the final sum of this list is <code>[[[[1,1],[2,2]],[3,3]],[4,4]]</code>:</p>
<pre><code>[1,1]
[2,2]
[3,3]
[4,4]
</code></pre>
<p>The final sum of this list is <code>[[[[3,0],[5,3]],[4,4]],[5,5]]</code>:</p>
<pre><code>[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
</code></pre>
<p>The final sum of this list is <code>[[[[5,0],[7,4]],[5,5]],[6,6]]</code>:</p>
<pre><code>[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]
</code></pre>
<p>Here's a slightly larger example:</p>
<pre><code>[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]
</code></pre>
<p>The final sum <code>[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]</code> is found after adding up the above snailfish numbers:</p>
<pre><code>  [[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
+ [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
= [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]

[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]
+ [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
  = [[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]

  [[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]
+ [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
  = [[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]

  [[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]
+ [7,[5,[[3,8],[1,4]]]]
  = [[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]

  [[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]
+ [[2,[2,2]],[8,[8,1]]]
  = [[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]

  [[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]
+ [2,9]
  = [[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]

  [[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]
+ [1,[[[9,3],9],[[9,0],[0,7]]]]
  = [[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]

  [[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]
+ [[[5,[7,4]],7],1]
  = [[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]

  [[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]
+ [[[[4,2],2],6],[8,7]]
  = [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]
  </code></pre>
<p>To check whether it's the right answer, the snailfish teacher only checks the <strong>magnitude</strong> of the final sum. The magnitude of a pair is 3 times the magnitude of its left element plus 2 times the magnitude of its right element. The magnitude of a regular number is just that number.</p>
<p>For example, the magnitude of <code>[9,1]</code> is <code>3*9 + 2*1 = <strong>29</strong></code>; the magnitude of <code>[1,9]</code> is <code>3*1 + 2*9 = <strong>21</strong></code>. Magnitude calculations are recursive: the magnitude of <code>[[9,1],[1,9]]</code> is <code>3*29 + 2*21 = <strong>129</strong></code>.</p>
<p>Here are a few more magnitude examples:</p>
<ul>
<li><code>[[1,2],[[3,4],5]]</code> becomes <code><strong>143</strong></code>.</li>
<li><code>[[[[0,7],4],[[7,8],[6,0]]],[8,1]]</code> becomes <code><strong>1384</strong></code>.</li>
<li><code>[[[[1,1],[2,2]],[3,3]],[4,4]]</code> becomes <code><strong>445</strong></code>.</li>
<li><code>[[[[3,0],[5,3]],[4,4]],[5,5]]</code> becomes <code><strong>791</strong></code>.</li>
<li><code>[[[[5,0],[7,4]],[5,5]],[6,6]]</code> becomes <code><strong>1137</strong></code>.</li>
<li><code>[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]</code> becomes <code><strong>3488</strong></code>.</li>
</ul>
<p>So, given this example homework assignment:</p>
<pre><code>[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
</code></pre>
<p>The final sum is:</p>
<pre><code>[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]</code></pre>
<p>The magnitude of this final sum is <code><strong>4140</strong></code>.</p>
<p>Add up all of the snailfish numbers from the homework assignment in the order they appear. <strong>What is the magnitude of the final sum?</strong></p>
</article>
