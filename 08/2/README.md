# Day 8 - Part 2

<article class="day-desc"><p>Through a little deduction, you should now be able to determine the remaining digits. Consider again the first example above:</p>
<pre><code>acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
cdfeb fcadb cdfeb cdbaf</code></pre>
<p>After some careful analysis, the mapping between signal wires and segments only make sense in the following configuration:</p>
<pre><code> dddd
e    a
e    a
 ffff
g    b
g    b
 cccc
</code></pre>
<p>So, the unique signal patterns would correspond to the following digits:</p>
<ul>
<li><code>acedgfb</code>: <code>8</code></li>
<li><code>cdfbe</code>: <code>5</code></li>
<li><code>gcdfa</code>: <code>2</code></li>
<li><code>fbcad</code>: <code>3</code></li>
<li><code>dab</code>: <code>7</code></li>
<li><code>cefabd</code>: <code>9</code></li>
<li><code>cdfgeb</code>: <code>6</code></li>
<li><code>eafb</code>: <code>4</code></li>
<li><code>cagedb</code>: <code>0</code></li>
<li><code>ab</code>: <code>1</code></li>
</ul>
<p>Then, the four digits of the output value can be decoded:</p>
<ul>
<li><code>cdfeb</code>: <code><strong>5</strong></code></li>
<li><code>fcadb</code>: <code><strong>3</strong></code></li>
<li><code>cdfeb</code>: <code><strong>5</strong></code></li>
<li><code>cdbaf</code>: <code><strong>3</strong></code></li>
</ul>
<p>Therefore, the output value for this entry is <code><strong>5353</strong></code>.</p>
<p>Following this same process for each entry in the second, larger example above, the output value of each entry can be determined:</p>
<ul>
<li><code>fdgacbe cefdb cefbgd gcbe</code>: <code>8394</code></li>
<li><code>fcgedb cgb dgebacf gc</code>: <code>9781</code></li>
<li><code>cg cg fdcagb cbg</code>: <code>1197</code></li>
<li><code>efabcd cedba gadfec cb</code>: <code>9361</code></li>
<li><code>gecf egdcabf bgf bfgea</code>: <code>4873</code></li>
<li><code>gebdcfa ecba ca fadegcb</code>: <code>8418</code></li>
<li><code>cefg dcbef fcge gbcadfe</code>: <code>4548</code></li>
<li><code>ed bcgafe cdgba cbgef</code>: <code>1625</code></li>
<li><code>gbdfcae bgc cg cgb</code>: <code>8717</code></li>
<li><code>fgae cfgab fg bagce</code>: <code>4315</code></li>
</ul>
<p>Adding all of the output values in this larger example produces <code><strong>61229</strong></code>.</p>
<p>For each entry, determine all of the wire/segment connections and decode the four-digit output values. <strong>What do you get if you add up all of the output values?</strong></p>
</article>
