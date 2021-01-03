reply to query on sample 2 January 2021.Rmd
-------------------------------------------

### Alan E. Berger

### version 1

### available at <https://github.com/AlanBerger/R_Programming_GitHub_files>

There are a number of ways to get multiple vectors of samples with or without replacement from a given vector. In the example below I'll in effect get 1000 vectors, each containing 25 samples taken **with replacement** from the set of numbers 1:10 (with replacement meaning the same number can occur more than once in a sample). One way is in a for loop, using the **sample** function, and another is (as done in the Statistical Inference Course in the Johns Hopkins University Data Science Specialization on Coursera) to get 1000 \* 25 samples "all at once" and place these values in a matrix. The rows or the columns of the matrix can be used as the individual samples. In the Statistical Inference Course (whose textbook is "Statistical inference for data science", Brian Caffo, Leanpub, last updated on 2016-05-24, <https://leanpub.com/LittleInferenceBook>), the rows of the matrix are used for the individual samples (here containing 25 values).

Since the default order of entries in a matrix are filled column by column, for example

    m <- matrix(1:12, nrow = 6, ncol = 2)
    m
         [,1] [,2]
    [1,]    1    7
    [2,]    2    8
    [3,]    3    9
    [4,]    4   10
    [5,]    5   11
    [6,]    6   12

in order to show that these two methods for getting multiple samples give the same result, I am going to use the columns of the matrix as the individual samples. So then I want the matrix to have 1000 columns with the row size equal 25.

In order to be able to reproduce a calculation that uses random numbers, it is essential that the user set the initial seed for the random number generation, via `set.seed(some.arbitrary.initial.value)`, for example `set.seed(1234)`, once, before the start of the particular calculation. Otherwise R will use some internal value and the calculation could not in general be exactly replicated.

Here are examples:

``` r
set.seed(1234)

#### use a for loop

# get 1000 samples of numbers in 1:10 each of size 25, with replacement
# and take the mean of each sample, and then do a histogram plot of
# these 1000 means

v <- numeric(1000)
for (k in 1:1000) {
   s <- sample(1:10, size = 25, replace = TRUE)
   v[k] <- mean(s) 
}
# print the last 25 samples
s
```

    ##  [1]  3  6  1 10  6  5 10  2 10 10  3  5  9  8  9  2  1  4  3  1  3  9 10 10  1

``` r
# print the first 10 means
v[1:10]
```

    ##  [1] 5.80 5.48 6.36 5.08 6.08 5.48 5.36 5.12 5.52 5.52

``` r
# plot the histogram
# find the range of v to determine the x-axis range
range(v)
```

    ## [1] 3.84 7.64

Do a histogram plot of the means

``` r
# specify the break points of the histogram
breakpts <- seq(3, 8, 0.25)
hist(v, breaks = breakpts)  # the peak seems to be around 5.5 as it should be 
```

![histogram of sampled means](https://github.com/AlanBerger/R_Programming_GitHub_files/blob/master/reply-to-query-on-sample-2-Jan-2021-chunk.png)

Now use a matrix to get the samples

``` r
# Since I want to reproduce the results above, I need to again use set.seed
# with the same initial seed value

set.seed(1234)

# want the row dimension of the matrix to be 25, and the number of columns to be 1000, so
# each of the 1000 individual samples of size 25 will be a column of the matrix

m <- matrix(sample(1:10, size = 25*1000, replace = TRUE), nrow = 25, ncol = 1000)
# get the mean of each column of m (will be the same as v above)
vector.of.1000.means.from.m <- apply(m, 2, mean)

identical(v, vector.of.1000.means.from.m)  # check they are the same
```

    ## [1] TRUE

``` r
# and so we would get the same histogram as before

# print the last column of m
m[, 1000]  # should be the same as the value of s at the end of the loop above
```

    ##  [1]  3  6  1 10  6  5 10  2 10 10  3  5  9  8  9  2  1  4  3  1  3  9 10 10  1

``` r
identical(m[, 1000], s)  # indeed this is the case
```

    ## [1] TRUE

Hope this is helpful, Alan

Attributions and legal notices
------------------------------

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. To view a copy of this license, visit <https://creativecommons.org/licenses/by-nc-sa/4.0/> or send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA. There is a full version of this license at this web site: <https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode>

Note the reader should not infer any endorsement or recommendation or approval for the material in this article from any of the sources or persons cited above or any other entities mentioned in this article.
