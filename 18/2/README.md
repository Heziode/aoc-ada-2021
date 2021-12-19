# Day 18 - Part 2

<article class="day-desc"><p>You notice a second question on the back of the homework assignment:</p>
<p>What is the largest magnitude you can get from adding only two of the snailfish numbers?</p>
<p>Note that snailfish addition is not <a href="https://en.wikipedia.org/wiki/Commutative_property" target="_blank">commutative</a> - that is, <code>x + y</code> and <code>y + x</code> can produce different results.</p>
<p>Again considering the last example homework assignment above:</p>
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
<p>The largest magnitude of the sum of any two snailfish numbers in this list is <code><strong>3993</strong></code>. This is the magnitude of <code>[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]</code> + <code>[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]</code>, which reduces to <code>[[[[7,8],[6,6]],[[6,0],[7,7]]],[[[7,8],[8,8]],[[7,9],[0,6]]]]</code>.</p>
<p><strong>What is the largest magnitude of any sum of two different snailfish numbers from the homework assignment?</strong></p>
</article>
