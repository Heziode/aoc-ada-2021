# Day 3 - Part 2

<article class="day-desc"><p>Next, you should verify the <strong>life support rating</strong>, which can be determined by multiplying the <strong>oxygen generator rating</strong> by the <strong>CO2 scrubber rating</strong>.</p>
<p>Both the oxygen generator rating and the CO2 scrubber rating are values that can be found in your diagnostic report - finding them is the tricky part. Both values are located using a similar process that involves filtering out values until only one remains. Before searching for either rating value, start with the full list of binary numbers from your diagnostic report and <strong>consider just the first bit</strong> of those numbers. Then:</p>
<ul>
<li>Keep only numbers selected by the <strong>bit criteria</strong> for the type of rating value for which you are searching. Discard numbers which do not match the bit criteria.</li>
<li>If you only have one number left, stop; this is the rating value for which you are searching.</li>
<li>Otherwise, repeat the process, considering the next bit to the right.</li>
</ul>
<p>The <strong>bit criteria</strong> depends on which type of rating value you want to find:</p>
<ul>
<li>To find <strong>oxygen generator rating</strong>, determine the <strong>most common</strong> value (<code>0</code> or <code>1</code>) in the current bit position, and keep only numbers with that bit in that position. If <code>0</code> and <code>1</code> are equally common, keep values with a <code><strong>1</strong></code> in the position being considered.</li>
<li>To find <strong>CO2 scrubber rating</strong>, determine the <strong>least common</strong> value (<code>0</code> or <code>1</code>) in the current bit position, and keep only numbers with that bit in that position. If <code>0</code> and <code>1</code> are equally common, keep values with a <code><strong>0</strong></code> in the position being considered.</li>
</ul>
<p>For example, to determine the <strong>oxygen generator rating</strong> value using the same example diagnostic report from above:</p>
<ul>
<li>Start with all 12 numbers and consider only the first bit of each number. There are more <code>1</code> bits (7) than <code>0</code> bits (5), so keep only the 7 numbers with a <code>1</code> in the first position: <code>11110</code>, <code>10110</code>, <code>10111</code>, <code>10101</code>, <code>11100</code>, <code>10000</code>, and <code>11001</code>.</li>
<li>Then, consider the second bit of the 7 remaining numbers: there are more <code>0</code> bits (4) than <code>1</code> bits (3), so keep only the 4 numbers with a <code>0</code> in the second position: <code>10110</code>, <code>10111</code>, <code>10101</code>, and <code>10000</code>.</li>
<li>In the third position, three of the four numbers have a <code>1</code>, so keep those three: <code>10110</code>, <code>10111</code>, and <code>10101</code>.</li>
<li>In the fourth position, two of the three numbers have a <code>1</code>, so keep those two: <code>10110</code> and <code>10111</code>.</li>
<li>In the fifth position, there are an equal number of <code>0</code> bits and <code>1</code> bits (one each). So, to find the <strong>oxygen generator rating</strong>, keep the number with a <code>1</code> in that position: <code>10111</code>.</li>
<li>As there is only one number left, stop; the <strong>oxygen generator rating</strong> is <code>10111</code>, or <code><strong>23</strong></code> in decimal.</li>
</ul>
<p>Then, to determine the <strong>CO2 scrubber rating</strong> value from the same example above:</p>
<ul>
<li>Start again with all 12 numbers and consider only the first bit of each number. There are fewer <code>0</code> bits (5) than <code>1</code> bits (7), so keep only the 5 numbers with a <code>0</code> in the first position: <code>00100</code>, <code>01111</code>, <code>00111</code>, <code>00010</code>, and <code>01010</code>.</li>
<li>Then, consider the second bit of the 5 remaining numbers: there are fewer <code>1</code> bits (2) than <code>0</code> bits (3), so keep only the 2 numbers with a <code>1</code> in the second position: <code>01111</code> and <code>01010</code>.</li>
<li>In the third position, there are an equal number of <code>0</code> bits and <code>1</code> bits (one each). So, to find the <strong>CO2 scrubber rating</strong>, keep the number with a <code>0</code> in that position: <code>01010</code>.</li>
<li>As there is only one number left, stop; the <strong>CO2 scrubber rating</strong> is <code>01010</code>, or <code><strong>10</strong></code> in decimal.</li>
</ul>
<p>Finally, to find the life support rating, multiply the oxygen generator rating (<code>23</code>) by the CO2 scrubber rating (<code>10</code>) to get <code><strong>230</strong></code>.</p>
<p>Use the binary numbers in your diagnostic report to calculate the oxygen generator rating and CO2 scrubber rating, then multiply them together. <strong>What is the life support rating of the submarine?</strong> (Be sure to represent your answer in decimal, not binary.)</p>
</article>