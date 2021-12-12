# Day 12 - Part 2

<article class="day-desc"><p>After reviewing the available paths, you realize you might have time to visit a single small cave <strong>twice</strong>. Specifically, big caves can be visited any number of times, a single small cave can be visited at most twice, and the remaining small caves can be visited at most once. However, the caves named <code>start</code> and <code>end</code> can only be visited <strong>exactly once each</strong>: once you leave the <code>start</code> cave, you may not return to it, and once you reach the <code>end</code> cave, the path must end immediately.</p>
<p>Now, the <code>36</code> possible paths through the first example above are:</p>
<pre><code>start,A,b,A,b,A,c,A,end
start,A,b,A,b,A,end
start,A,b,A,b,end
start,A,b,A,c,A,b,A,end
start,A,b,A,c,A,b,end
start,A,b,A,c,A,c,A,end
start,A,b,A,c,A,end
start,A,b,A,end
start,A,b,d,b,A,c,A,end
start,A,b,d,b,A,end
start,A,b,d,b,end
start,A,b,end
start,A,c,A,b,A,b,A,end
start,A,c,A,b,A,b,end
start,A,c,A,b,A,c,A,end
start,A,c,A,b,A,end
start,A,c,A,b,d,b,A,end
start,A,c,A,b,d,b,end
start,A,c,A,b,end
start,A,c,A,c,A,b,A,end
start,A,c,A,c,A,b,end
start,A,c,A,c,A,end
start,A,c,A,end
start,A,end
start,b,A,b,A,c,A,end
start,b,A,b,A,end
start,b,A,b,end
start,b,A,c,A,b,A,end
start,b,A,c,A,b,end
start,b,A,c,A,c,A,end
start,b,A,c,A,end
start,b,A,end
start,b,d,b,A,c,A,end
start,b,d,b,A,end
start,b,d,b,end
start,b,end
</code></pre>
<p>The slightly larger example above now has <code>103</code> paths through it, and the even larger example now has <code>3509</code> paths through it.</p>
<p>Given these new rules, <strong>how many paths through this cave system are there?</strong></p>
</article>
