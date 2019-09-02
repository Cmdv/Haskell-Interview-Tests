#Compare the Triplets

Alice and Bob each created one problem. A reviewer rates the two challenges, awarding points on a scale from 1 to 100 for three categories: problem clarity, originality, and difficulty.

We define the rating for Alice's challenge to be the triplet `a == (a0, a1, a2)`, and the rating for Bob's challenge to be the triplet `b == (b0, b1, b2)`.

Your task is to find their comparison points by comparing `a0` with `b0` , `a1` with `b1` and `a2` with `b2`.

If `a1 > b1`, then Alice is awarded  point.
If `a1 < b1`, then Bob is awarded  point.
If `a1 == b1`, then neither person receives a point.

Comparison points is the total points a person earned.

Given `a` and `b` , determine their respective comparison points.

For example, `a == [1,2,3]` and `b == [3,2,1]`. For elements `0`, Bob is awarded a point because `a0 < b0`. For the equal elements `a1` and `b1`, no points are earned. Finally, for elements `2`, `a2 > b2` so Alice receives a point. Your return array would be `[1,1]` with Alice's score first and Bob's second.

##Function Description

Complete the function compareTriplets in the editor below. It must return an array of two integers, the first being Alice's score and the second being Bob's.

compareTriplets has the following parameter(s):

a: an array of integers representing Alice's challenge rating
b: an array of integers representing Bob's challenge rating


##Sample Input / Output
```
> compareTriplets [5 6 7] [3 6 10]
[1, 1]

> campareTriplets [17, 28, 30] [99, 16, 8]
[2, 1]
```


#Diagonal Difference

Given a square matrix, calculate the absolute difference between the sums of its diagonals.

For example, the square matrix `arr`is shown below:

```
1 2 3
4 5 6
9 8 9
```

The left-to-right diagonal = `1 + 5 + 9 == 15`. The right to left diagonal = `3 + 5 + 9 == 17` . Their absolute difference is `[15 -- 17] == 2`.

```
> diagonalDifference [3, 11, 2, 4, 4, 5, 6, 10, 8, -12]
-- 3 being the matrix dimmension (3x3) then the rest of the list is the content of matrix
```
