# Day 16 - Part 2

<article class="day-desc"><p>Now that you have the structure of your transmission decoded, you can calculate the value of the expression it represents.</p>
<p>Literal values (type ID <code>4</code>) represent a single number as described above. The remaining type IDs are more interesting:</p>
<ul>
<li>Packets with type ID <code>0</code> are <strong>sum</strong> packets - their value is the sum of the values of their sub-packets. If they only have a single sub-packet, their value is the value of the sub-packet.</li>
<li>Packets with type ID <code>1</code> are <strong>product</strong> packets - their value is the result of multiplying together the values of their sub-packets. If they only have a single sub-packet, their value is the value of the sub-packet.</li>
<li>Packets with type ID <code>2</code> are <strong>minimum</strong> packets - their value is the minimum of the values of their sub-packets.</li>
<li>Packets with type ID <code>3</code> are <strong>maximum</strong> packets - their value is the maximum of the values of their sub-packets.</li>
<li>Packets with type ID <code>5</code> are <strong>greater than</strong> packets - their value is <strong>1</strong> if the value of the first sub-packet is greater than the value of the second sub-packet; otherwise, their value is <strong>0</strong>. These packets always have exactly two sub-packets.</li>
<li>Packets with type ID <code>6</code> are <strong>less than</strong> packets - their value is <strong>1</strong> if the value of the first sub-packet is less than the value of the second sub-packet; otherwise, their value is <strong>0</strong>. These packets always have exactly two sub-packets.</li>
<li>Packets with type ID <code>7</code> are <strong>equal to</strong> packets - their value is <strong>1</strong> if the value of the first sub-packet is equal to the value of the second sub-packet; otherwise, their value is <strong>0</strong>. These packets always have exactly two sub-packets.</li>
</ul>
<p>Using these rules, you can now work out the value of the outermost packet in your BITS transmission.</p>
<p>For example:</p>
<ul>
<li><code>C200B40A82</code> finds the sum of <code>1</code> and <code>2</code>, resulting in the value <code><strong>3</strong></code>.</li>
<li><code>04005AC33890</code> finds the product of <code>6</code> and <code>9</code>, resulting in the value <code><strong>54</strong></code>.</li>
<li><code>880086C3E88112</code> finds the minimum of <code>7</code>, <code>8</code>, and <code>9</code>, resulting in the value <code><strong>7</strong></code>.</li>
<li><code>CE00C43D881120</code> finds the maximum of <code>7</code>, <code>8</code>, and <code>9</code>, resulting in the value <code><strong>9</strong></code>.</li>
<li><code>D8005AC2A8F0</code> produces <code>1</code>, because <code>5</code> is less than <code>15</code>.</li>
<li><code>F600BC2D8F</code> produces <code>0</code>, because <code>5</code> is not greater than <code>15</code>.</li>
<li><code>9C005AC2F8F0</code> produces <code>0</code>, because <code>5</code> is not equal to <code>15</code>.</li>
<li><code>9C0141080250320F1802104A08</code> produces <code>1</code>, because <code>1</code> + <code>3</code> = <code>2</code> * <code>2</code>.</li>
</ul>
<p><strong>What do you get if you evaluate the expression represented by your hexadecimal-encoded BITS transmission?</strong></p>
</article>
