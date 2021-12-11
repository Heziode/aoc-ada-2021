# Day 2 - Part 2

<article class="day-desc"><p>Based on your calculations, the planned course doesn't seem to make any sense. You find the submarine manual and discover that the process is actually slightly more complicated.</p>
<p>In addition to horizontal position and depth, you'll also need to track a third value, <strong>aim</strong>, which also starts at <code>0</code>. The commands also mean something entirely different than you first thought:</p>
<ul>
<li><code>down X</code> <strong>increases</strong> your aim by <code>X</code> units.</li>
<li><code>up X</code> <strong>decreases</strong> your aim by <code>X</code> units.</li>
<li><code>forward X</code> does two things:<ul>
  <li>It increases your horizontal position by <code>X</code> units.</li>
  <li>It increases your depth by your aim <strong>multiplied by</strong> <code>X</code>.</li>
</ul></li>
</ul>
<p>Again note that since you're on a submarine, <code>down</code> and <code>up</code> do the opposite of what you might expect: "down" means aiming in the positive direction.</p>
<p>Now, the above example does something different:</p>
<ul>
<li><code>forward 5</code> adds <code>5</code> to your horizontal position, a total of <code>5</code>. Because your aim is <code>0</code>, your depth does not change.</li>
<li><code>down 5</code> adds <code>5</code> to your aim, resulting in a value of <code>5</code>.</li>
<li><code>forward 8</code> adds <code>8</code> to your horizontal position, a total of <code>13</code>. Because your aim is <code>5</code>, your depth increases by <code>8*5=40</code>.</li>
<li><code>up 3</code> decreases your aim by <code>3</code>, resulting in a value of <code>2</code>.</li>
<li><code>down 8</code> adds <code>8</code> to your aim, resulting in a value of <code>10</code>.</li>
<li><code>forward 2</code> adds <code>2</code> to your horizontal position, a total of <code>15</code>.  Because your aim is <code>10</code>, your depth increases by <code>2*10=20</code> to a total of <code>60</code>.</li>
</ul>
<p>After following these new instructions, you would have a horizontal position of <code>15</code> and a depth of <code>60</code>. (Multiplying these produces <code><strong>900</strong></code>.)</p>
<p>Using this new interpretation of the commands, calculate the horizontal position and depth you would have after following the planned course. <strong>What do you get if you multiply your final horizontal position by your final depth?</strong></p>
</article>
