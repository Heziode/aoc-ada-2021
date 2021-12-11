# Day 11 - Part 1

<article class="day-desc"><p>You enter a large cavern full of rare bioluminescent <a href="https://www.youtube.com/watch?v=eih-VSaS2g0" target="_blank">dumbo octopuses</a>! They seem to not like the Christmas lights on your submarine, so you turn them off for now.</p>
<p>There are 100 <span title="I know it's weird; I grew up saying 'octopi' too.">octopuses</span> arranged neatly in a 10 by 10 grid. Each octopus slowly gains <stong>energy</stong> over time and <stong>flashes</stong> brightly for a moment when its energy is full. Although your lights are off, maybe you could navigate through the cave without disturbing the octopuses if you could predict when the flashes of light will happen.</p>
<p>Each octopus has an <stong>energy level</stong> - your submarine can remotely measure the energy level of each octopus (your puzzle input). For example:</p>
<pre><code>5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
</code></pre>
<p>The energy level of each octopus is a value between <code>0</code> and <code>9</code>. Here, the top-left octopus has an energy level of <code>5</code>, the bottom-right one has an energy level of <code>6</code>, and so on.</p>
<p>You can model the energy levels and flashes of light in <stong>steps</stong>. During a single step, the following occurs:</p>
<ul>
<li>First, the energy level of each octopus increases by <code>1</code>.</li>
<li>Then, any octopus with an energy level greater than <code>9</code> <stong>flashes</stong>. This increases the energy level of all adjacent octopuses by <code>1</code>, including octopuses that are diagonally adjacent. If this causes an octopus to have an energy level greater than <code>9</code>, it <stong>also flashes</stong>. This process continues as long as new octopuses keep having their energy level increased beyond <code>9</code>. (An octopus can only flash <stong>at most once per step</stong>.)</li>
<li>Finally, any octopus that flashed during this step has its energy level set to <code>0</code>, as it used all of its energy to flash.</li>
</ul>
<p>Adjacent flashes can cause an octopus to flash on a step even if it begins that step with very little energy. Consider the middle octopus with <code>1</code> energy in this situation:</p>
<pre><code>Before any steps:
11111
19991
19191
19991
11111

After step 1:
34543
4<stong>000</stong>4
5<stong>000</stong>5
4<stong>000</stong>4
34543

After step 2:
45654
51115
61116
51115
45654
</code></pre>
<p>An octopus is <stong>highlighted</stong> when it flashed during the given step.</p>
<p>Here is how the larger example above progresses:</p>
<pre><code>Before any steps:
5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526

After step 1:
6594254334
3856965822
6375667284
7252447257
7468496589
5278635756
3287952832
7993992245
5957959665
6394862637

After step 2:
88<stong>0</stong>7476555
5<stong>0</stong>89<stong>0</stong>87<stong>0</stong>54
85978896<stong>0</stong>8
84857696<stong>00</stong>
87<stong>00</stong>9<stong>0</stong>88<stong>00</stong>
66<stong>000</stong>88989
68<stong>0000</stong>5943
<stong>000000</stong>7456
9<stong>000000</stong>876
87<stong>0000</stong>6848

After step 3:
<stong>00</stong>5<stong>0</stong>9<stong>00</stong>866
85<stong>00</stong>8<stong>00</stong>575
99<stong>000000</stong>39
97<stong>000000</stong>41
9935<stong>0</stong>8<stong>00</stong>63
77123<stong>00000</stong>
791125<stong>000</stong>9
221113<stong>0000</stong>
<stong>0</stong>421125<stong>000</stong>
<stong>00</stong>21119<stong>000</stong>

After step 4:
2263<stong>0</stong>31977
<stong>0</stong>923<stong>0</stong>31697
<stong>00</stong>3222115<stong>0</stong>
<stong>00</stong>41111163
<stong>00</stong>76191174
<stong>00</stong>53411122
<stong>00</stong>4236112<stong>0</stong>
5532241122
1532247211
113223<stong>0</stong>211

After step 5:
4484144<stong>000</stong>
2<stong>0</stong>44144<stong>000</stong>
2253333493
1152333274
11873<stong>0</stong>3285
1164633233
1153472231
6643352233
2643358322
2243341322

After step 6:
5595255111
3155255222
33644446<stong>0</stong>5
2263444496
2298414396
2275744344
2264583342
7754463344
3754469433
3354452433

After step 7:
67<stong>0</stong>7366222
4377366333
4475555827
34966557<stong>0</stong>9
35<stong>00</stong>6256<stong>0</stong>9
35<stong>0</stong>9955566
3486694453
8865585555
486558<stong>0</stong>644
4465574644

After step 8:
7818477333
5488477444
5697666949
46<stong>0</stong>876683<stong>0</stong>
473494673<stong>0</stong>
474<stong>00</stong>97688
69<stong>0000</stong>7564
<stong>000000</stong>9666
8<stong>00000</stong>4755
68<stong>0000</stong>7755

After step 9:
9<stong>0</stong>6<stong>0000</stong>644
78<stong>00000</stong>976
69<stong>000000</stong>8<stong>0</stong>
584<stong>00000</stong>82
5858<stong>0000</stong>93
69624<stong>00000</stong>
8<stong>0</stong>2125<stong>000</stong>9
222113<stong>000</stong>9
9111128<stong>0</stong>97
7911119976

After step 10:
<stong>0</stong>481112976
<stong>00</stong>31112<stong>00</stong>9
<stong>00</stong>411125<stong>0</stong>4
<stong>00</stong>811114<stong>0</stong>6
<stong>00</stong>991113<stong>0</stong>6
<stong>00</stong>93511233
<stong>0</stong>44236113<stong>0</stong>
553225235<stong>0</stong>
<stong>0</stong>53225<stong>0</stong>6<stong>00</stong>
<stong>00</stong>3224<stong>0000</stong>
</code></pre>

<p>After step 10, there have been a total of <code>204</code> flashes. Fast forwarding, here is the same configuration every 10 steps:</p>

<pre><code>After step 20:
3936556452
56865568<stong>0</stong>6
449655569<stong>0</stong>
444865558<stong>0</stong>
445686557<stong>0</stong>
568<stong>00</stong>86577
7<stong>00000</stong>9896
<stong>0000000</stong>344
6<stong>000000</stong>364
46<stong>0000</stong>9543

After step 30:
<stong>0</stong>643334118
4253334611
3374333458
2225333337
2229333338
2276733333
2754574565
5544458511
9444447111
7944446119

After step 40:
6211111981
<stong>0</stong>421111119
<stong>00</stong>42111115
<stong>000</stong>3111115
<stong>000</stong>3111116
<stong>00</stong>65611111
<stong>0</stong>532351111
3322234597
2222222976
2222222762

After step 50:
9655556447
48655568<stong>0</stong>5
448655569<stong>0</stong>
445865558<stong>0</stong>
457486557<stong>0</stong>
57<stong>000</stong>86566
6<stong>00000</stong>9887
8<stong>000000</stong>533
68<stong>00000</stong>633
568<stong>0000</stong>538

After step 60:
25333342<stong>00</stong>
274333464<stong>0</stong>
2264333458
2225333337
2225333338
2287833333
3854573455
1854458611
1175447111
1115446111

After step 70:
8211111164
<stong>0</stong>421111166
<stong>00</stong>42111114
<stong>000</stong>4211115
<stong>0000</stong>211116
<stong>00</stong>65611111
<stong>0</stong>532351111
7322235117
5722223475
4572222754

After step 80:
1755555697
59655556<stong>0</stong>9
448655568<stong>0</stong>
445865558<stong>0</stong>
457<stong>0</stong>86557<stong>0</stong>
57<stong>000</stong>86566
7<stong>00000</stong>8666
<stong>0000000</stong>99<stong>0</stong>
<stong>0000000</stong>8<stong>00</stong>
<stong>0000000000</stong>

After step 90:
7433333522
2643333522
2264333458
2226433337
2222433338
2287833333
2854573333
4854458333
3387779333
3333333333

After step 100:
<stong>0</stong>397666866
<stong>0</stong>749766918
<stong>00</stong>53976933
<stong>000</stong>4297822
<stong>000</stong>4229892
<stong>00</stong>53222877
<stong>0</stong>532222966
9322228966
7922286866
6789998766
</code></pre>
<p>After 100 steps, there have been a total of <code><stong>1656</stong></code> flashes.</p>
<p>Given the starting energy levels of the dumbo octopuses in your cavern, simulate 100 steps. <stong>How many total flashes are there after 100 steps?</stong></p>
</article>
