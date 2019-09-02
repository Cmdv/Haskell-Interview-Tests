The Lost Islands of SpiralWorld
You just got the call. They finally found it. They found the ancient scroll of the lost islands and now they need you to make sense of it all.

The scroll is one long sequence of ~ and # characters. We know that ~ represents water while # can be a village or water.

The population of each village is the number of water cells before it. Eg. if we have ~~~# then # is a village with a population of 3. If a # has 0 water cells before it then it is actually a water cell. Eg. if we have ~## then the first # is a village of 1 while the second # is water. If a village has more that 9 water cells before it then we take the last digit of the number of water cells as the population. Eg. if we have ~~~~~~~~~~# then that # is actually water as the number of water cells before it is 10, hence the last digit is 0. Please note that # cells that turned out to be water cells do not count towards village populations. Eg. #~~~# represents a single village of 3 as opposed to a single village of 4.

Once you have identified all the actual villages on the scroll, the next obvious step (we live on SpiralWorld after all!) is to make a clockwise spiral out of it starting with a right step, ie.:

             789
123456789 -> 612
             543
Once done you finally have the map of the lost islands! Eg. if the original sequence was:

#~~~##~~#~~###~#~~##~#~#~

then its corresponding map is:

~1~1~
~~~2~
2~~~~
~3~~2
~1~~~
An island consists of touching villages. Eg. the map above has 4 islands: 1, (1,2), (2,3,1) and 2. The population of an island is the sum of the population of its villages, eg.: 1, 3, 6, 2. Your final task is to identity the most populated island on the map. You and your team thinks that this will give you the best chance to explore these long lost lands. On the above map the most populated island has a population of 6 (2+3+1). If a map has more than one island with the same population then it is enough to identity one of them.

To recap, the input to your program is a file with a sequence of ~ and # characters. The output should be a number that is the population of the most populated island.
