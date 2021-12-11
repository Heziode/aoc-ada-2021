# Day 7 - Part 2

<article class="day-desc"><p>The crabs don't seem interested in your proposed solution. Perhaps you misunderstand crab engineering?</p>
<p>As it turns out, crab submarine engines <span title="This appears to be due to the modial interaction of magneto-reluctance and capacitive duractance.">don't burn fuel at a constant rate</span>. Instead, each change of 1 step in horizontal position costs 1 more unit of fuel than the last: the first step costs <code>1</code>, the second step costs <code>2</code>, the third step costs <code>3</code>, and so on.</p>
<p>As each crab moves, moving further becomes more expensive. This changes the best horizontal position to align them all on; in the example above, this becomes <code>5</code>:</p>
<ul>
<li>Move from <code>16</code> to <code>5</code>: <code>66</code> fuel</li>
<li>Move from <code>1</code> to <code>5</code>: <code>10</code> fuel</li>
<li>Move from <code>2</code> to <code>5</code>: <code>6</code> fuel</li>
<li>Move from <code>0</code> to <code>5</code>: <code>15</code> fuel</li>
<li>Move from <code>4</code> to <code>5</code>: <code>1</code> fuel</li>
<li>Move from <code>2</code> to <code>5</code>: <code>6</code> fuel</li>
<li>Move from <code>7</code> to <code>5</code>: <code>3</code> fuel</li>
<li>Move from <code>1</code> to <code>5</code>: <code>10</code> fuel</li>
<li>Move from <code>2</code> to <code>5</code>: <code>6</code> fuel</li>
<li>Move from <code>14</code> to <code>5</code>: <code>45</code> fuel</li>
</ul>
<p>This costs a total of <code><strong>168</strong></code> fuel. This is the new cheapest possible outcome; the old alignment position (<code>2</code>) now costs <code>206</code> fuel instead.</p>
<p>Determine the horizontal position that the crabs can align to using the least fuel possible so they can make you an escape route! <strong>How much fuel must they spend to align to that position?</strong></p>
</article>