# Day 10 - Part 2

<article class="day-desc"><h2 id="part2">--- Part Two ---</h2><p>Now, discard the corrupted lines.  The remaining lines are <strong>incomplete</strong>.</p>
<p>Incomplete lines don't have any incorrect characters - instead, they're missing some closing characters at the end of the line. To repair the navigation subsystem, you just need to figure out <strong>the sequence of closing characters</strong> that complete all open chunks in the line.</p>
<p>You can only use closing characters (<code>)</code>, <code>]</code>, <code>}</code>, or <code>&gt;</code>), and you must add them in the correct order so that only legal pairs are formed and all chunks end up closed.</p>
<p>In the example above, there are five incomplete lines:</p>
<ul>
<li><code>[({(&lt;(())[]&gt;[[{[]{&lt;()&lt;&gt;&gt;</code> - Complete by adding <code>}}]])})]</code>.</li>
<li><code>[(()[&lt;&gt;])]({[&lt;{&lt;&lt;[]&gt;&gt;(</code> - Complete by adding <code>)}&gt;]})</code>.</li>
<li><code>(((({&lt;&gt;}&lt;{&lt;{&lt;&gt;}{[]{[]{}</code> - Complete by adding <code>}}&gt;}&gt;))))</code>.</li>
<li><code>{&lt;[[]]&gt;}&lt;{[{[{[]{()[[[]</code> - Complete by adding <code>]]}}]}]}&gt;</code>.</li>
<li><code>&lt;{([{{}}[&lt;[[[&lt;&gt;{}]]]&gt;[]]</code> - Complete by adding <code>])}&gt;</code>.</li>
</ul>
<p>Did you know that autocomplete tools <strong>also</strong> have contests? It's true! The score is determined by considering the completion string character-by-character. Start with a total score of <code>0</code>. Then, for each character, multiply the total score by 5 and then increase the total score by the point value given for the character in the following table:</p>
<ul>
<li><code>)</code>: <code>1</code> point.</li>
<li><code>]</code>: <code>2</code> points.</li>
<li><code>}</code>: <code>3</code> points.</li>
<li><code>&gt;</code>: <code>4</code> points.</li>
</ul>
<p>So, the last completion string above - <code>])}&gt;</code> - would be scored as follows:</p>
<ul>
<li>Start with a total score of <code>0</code>.</li>
<li>Multiply the total score by 5 to get <code>0</code>, then add the value of <code>]</code> (2) to get a new total score of <code>2</code>.</li>
<li>Multiply the total score by 5 to get <code>10</code>, then add the value of <code>)</code> (1) to get a new total score of <code>11</code>.</li>
<li>Multiply the total score by 5 to get <code>55</code>, then add the value of <code>}</code> (3) to get a new total score of <code>58</code>.</li>
<li>Multiply the total score by 5 to get <code>290</code>, then add the value of <code>&gt;</code> (4) to get a new total score of <code>294</code>.</li>
</ul>
<p>The five lines' completion strings have total scores as follows:</p>
<ul>
<li><code>}}]])})]</code> - <code>288957</code> total points.</li>
<li><code>)}&gt;]})</code> - <code>5566</code> total points.</li>
<li><code>}}&gt;}&gt;))))</code> - <code>1480781</code> total points.</li>
<li><code>]]}}]}]}&gt;</code> - <code>995444</code> total points.</li>
<li><code>])}&gt;</code> - <code>294</code> total points.</li>
</ul>
<p>Autocomplete tools are an odd bunch: the winner is found by <strong>sorting</strong> all of the scores and then taking the <strong>middle</strong> score. (There will always be an odd number of scores to consider.) In this example, the middle score is <code><strong>288957</strong></code> because there are the same number of scores smaller and larger than it.</p>
<p>Find the completion string for each incomplete line, score the completion strings, and sort the scores. <strong>What is the middle score?</strong></p>
</article>