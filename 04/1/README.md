# Day 4 - Part 1

<article class="day-desc"><p>You're already almost 1.5km (almost a mile) below the surface of the ocean, already so deep that you can't see any sunlight. What you <strong>can</strong> see, however, is a giant squid that has attached itself to the outside of your submarine.</p>
<p>Maybe it wants to play <a href="https://en.wikipedia.org/wiki/Bingo_(American_version)" target="_blank">bingo</a>?</p>
<p>Bingo is played on a set of boards each consisting of a 5x5 grid of numbers. Numbers are chosen at random, and the chosen number is <strong>marked</strong> on all boards on which it appears. (Numbers may not appear on all boards.) If all numbers in any row or any column of a board are marked, that board <strong>wins</strong>. (Diagonals don't count.)</p>
<p>The submarine has a <strong>bingo subsystem</strong> to help passengers (currently, you and the giant squid) pass the time. It automatically generates a random order in which to draw numbers and a random set of boards (your puzzle input). For example:</p>
<pre><code>7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
8  2 23  4 24
21  9 14 16  7
6 10  3 18  5
1 12 20 15 19

3 15  0  2 22
9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
2  0 12  3  7
</code></pre>
<p>After the first five numbers are drawn (<code>7</code>, <code>4</code>, <code>9</code>, <code>5</code>, and <code>11</code>), there are no winners, but the boards are marked as follows (shown here adjacent to each other to save space):</p>
<pre><code>22 13 17 <strong>11</strong>  0         3 15  0  2 22        14 21 17 24  <strong>4</strong>
 8  2 23  <strong>4</strong> 24         <strong>9</strong> 18 13 17  <strong>5</strong>        10 16 15  <strong>9</strong> 19
21  <strong>9</strong> 14 16  <strong>7</strong>        19  8  <strong>7</strong> 25 23        18  8 23 26 20
 6 10  3 18  <strong>5</strong>        20 <strong>11</strong> 10 24  <strong>4</strong>        22 <strong>11</strong> 13  6  <strong>5</strong>
 1 12 20 15 19        14 21 16 12  6         2  0 12  3  <strong>7</strong>
</code></pre>
<p>After the next six numbers are drawn (<code>17</code>, <code>23</code>, <code>2</code>, <code>0</code>, <code>14</code>, and <code>21</code>), there are still no winners:</p>
<pre><code>22 13 <strong>17</strong> <strong>11</strong>  <strong>0</strong>         3 15  <strong>0</strong>  <strong>2</strong> 22        <strong>14</strong> <strong>21</strong> <strong>17</strong> 24  <strong>4</strong>
 8  <strong>2</strong> <strong>23</strong>  <strong>4</strong> 24         <strong>9</strong> 18 13 <strong>17</strong>  <strong>5</strong>        10 16 15  <strong>9</strong> 19
<strong>21</strong>  <strong>9</strong> <strong>14</strong> 16  <strong>7</strong>        19  8  <strong>7</strong> 25 <strong>23</strong>        18  8 <strong>23</strong> 26 20
 6 10  3 18  <strong>5</strong>        20 <strong>11</strong> 10 24  <strong>4</strong>        22 <strong>11</strong> 13  6  <strong>5</strong>
 1 12 20 15 19        <strong>14</strong> <strong>21</strong> 16 12  6         <strong>2</strong>  <strong>0</strong> 12  3  <strong>7</strong>
</code></pre>
<p>Finally, <code>24</code> is drawn:</p>
<pre><code>22 13 <strong>17</strong> <strong>11</strong>  <strong>0</strong>         3 15  <strong>0</strong>  <strong>2</strong> 22        <strong>14</strong> <strong>21</strong> <strong>17</strong> <strong>24</strong>  <strong>4</strong>
 8  <strong>2</strong> <strong>23</strong>  <strong>4</strong> <strong>24</strong>         <strong>9</strong> 18 13 <strong>17</strong>  <strong>5</strong>        10 16 15  <strong>9</strong> 19
<strong>21</strong>  <strong>9</strong> <strong>14</strong> 16  <strong>7</strong>        19  8  <strong>7</strong> 25 <strong>23</strong>        18  8 <strong>23</strong> 26 20
 6 10  3 18  <strong>5</strong>        20 <strong>11</strong> 10 <strong>24</strong>  <strong>4</strong>        22 <strong>11</strong> 13  6  <strong>5</strong>
 1 12 20 15 19        <strong>14</strong> <strong>21</strong> 16 12  6         <strong>2</strong>  <strong>0</strong> 12  3  <strong>7</strong>
</code></pre>
<p>At this point, the third board <strong>wins</strong> because it has at least one complete row or column of marked numbers (in this case, the entire top row is marked: <code><strong>14 21 17 24  4</strong></code>).</p>
<p>The <strong>score</strong> of the winning board can now be calculated. Start by finding the <strong>sum of all unmarked numbers</strong> on that board; in this case, the sum is <code>188</code>. Then, multiply that sum by <strong>the number that was just called</strong> when the board won, <code>24</code>, to get the final score, <code>188 * 24 = <strong>4512</strong></code>.</p>
<p>To guarantee victory against the giant squid, figure out which board will win first. <strong>What will your final score be if you choose that board?</strong></p>
</article>
