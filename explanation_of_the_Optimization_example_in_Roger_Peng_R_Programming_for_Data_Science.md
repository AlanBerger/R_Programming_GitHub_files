"explanation of the Optimization example in Roger D Peng R Programming for Data Science.Rmd"
--------------------------------------------------------------------------------------------

### Alan E. Berger Sept 28, 2020

### version 1

### available at <https://github.com/AlanBerger/R_Programming_GitHub_files>

Introduction
------------

The Optimization example in Section 16.5 ("Application: Optimization") of Roger D. Peng, R Programming for Data Science, LeanPub, <https://leanpub.com/rprogramming>; has been the subject of a number of queries in the R Programming Course in the Johns Hopkins University Data Science Specialization on Coursera.

This "Application: Optimization" example is also available at: <https://github.com/DataScienceSpecialization/courses/blob/master/02_RProgramming/Scoping/index.md#application-optimization> **see the full attributions and legal notices pertaining to this web page at the bottom of this article**

For convenient reference I will sometimes refer to these sources as Peng \[1\] and web-opt \[1\].

In particular, the make.NegLogLik function given in the book and web location cited above uses lexical scoping and some clever logic to facilitate finding the (mu, sigma) values that maximize the likelihood function for estimating the normal distribution N(x; mu, sigma) that "best fits" some values V = c(v1, ... , vn) that are assumed to be n independent random samples from some (unknown) normal distribution. The maximum likelihood method determines (mu, sigma) by finding the (mu, sigma) that maximizes the likelihood function L(mu, sigma) which in this situation is the product of N(v1; mu, sigma) \* ... \* N(vn; mu, sigma)
The make.NegLogLik function also allows for fixing one of the mu, sigma parameters and fitting the other parameter to the data.

For more details on maximum likelihood see for example the Wikipedia article <https://en.wikipedia.org/wiki/Maximum_likelihood_estimation> (for maximum likelihood for the normal distribution scroll down to the section "Continuous distribution, continuous parameter space"); this is also covered in many statistics textbooks. I'll refer to this web page as web-wiki \[1\]

One way to see why L(mu, sigma) above is natural is to consider the probability that a randomly sampled value from the normal distribution N(x; mu, sigma) is in the interval \[v1 - d, v1 + d\] where d is a small positive constant. This probability, which equals the integral over this interval of N(x; mu, sigma),
is approximately 2d \* N(v1; mu, sigma). If the vector V above contains independent random samples, and using intervals I(i) of width 2d with vi at the center of I(i), for i = 1, ... , n, the probability of observing n successive independent random samples xi from a normal distribution with mean mu and standard deviation sigma with xi in I(i) for i = 1, ... , n is approximately (2d)^n times the product N(v1; mu, sigma) \* ... \* N(vn; mu, sigma)
The relative error in this approximation goes to 0 as d goes to zero, which "explains" why it is logical to maximize L(mu, sigma) given the vector of observed independent random sample values V.

Note there is actually a simple closed form solution to this optimization problem, given in the Wikipedia article above (mu = the mean of V, and sigma^2 = \[(n-1)/n\] \* S^2 where S^2 is the sample variance from V, var(V)). However in more general maximum likelihood calculations one needs to do numerical optimization to approximate the "best" parameters, and here the point of this example is to illustrate lexical scoping and function closure in R <there is discussion below of what a function closure is,  
and an appendix on function closure toward the end of this article>. This example also permits one to "fix" either mu or sigma and just optimize the likelihood function L(mu, sigma) over the other parameter. This adds a layer of complexity in order to be able to specify if one of the parameters is to be fixed and if so to provide the fixed value to the optimization.

Note since the logarithm function log(x) is strictly increasing for positive values of x, finding where the likelihood function attains its maximum is equivalent to finding where log(likelihood) attains its maximum (here log means the natural logarithm, log base *e*). The log of the likelihood function is often more convenient to deal with. Many optimization routines search for minima, so if we multiply log(likelihood) by -1 we can convert finding the location of the maximum of log(likelihood) to finding the location of the minimum of -log(likelihood) which accounts for the NegLogLik in the function name make.NegLogLik The make.NegLogLik function constructs the negative log likelihood function that the optimization routine will (approximately) find the minimum of.

### Here is the make.NegLogLik function copied from web-opt \[1\], see also Peng \[1\]

``` r
make.NegLogLik <- function(data, fixed=c(FALSE,FALSE)) {
        params <- fixed
        function(p) {
                params[!fixed] <- p
                mu <- params[1]
                sigma <- params[2]
                a <- -0.5*length(data)*log(2*pi*sigma^2)
                b <- -0.5*sum((data-mu)^2) / (sigma^2)
                -(a + b)
        }
 }
```

The argument **data** of make.NegLogLik will contain the n (approximately) indpendent random samples from some normal distribution (the sample values are denoted by V above, and **normals** in the Roger Peng book, Peng \[1\] and in the web location web-opt \[1\] cited above), and the argument **fixed** (which requires some detailed explanation) will "tell" make.NegLogLik whether one of mu and sigma is to be fixed (and the search conducted only over the other parameter) and if so, what is the fixed value. The default value of fixed, c(FALSE, FALSE), specifies a search over both mu and sigma.

If one does the following call (and using the notation **nLL** as in Peng \[1\], web-opt \[1\]) where V is the data vector as described above, and fixed.vector is a user chosen value for fixed:

nLL &lt;- make.NegLogLik(data = V, fixed = fixed.vector)

then nLL will be the function of mu or sigma or both whose minimum will be searched for by the optimization routine.

Now I'll endeavor to explain how the make.NegLogLik function returns the desired function in nLL, and how/why nLL retains the data in V (and if one of the parameters has been fixed, the fixed value) between calls to nLL from the optimization routine.

How the make.NegLogLik function works
-------------------------------------

The vector of length 2 called **fixed** is being used to indicate whether or not to keep (freeze, fix) the intial value of mu or of sigma and just search for the optimal value of the other parameter, or whether to search for the optimal value for both parameters. The optimal value is the location at which the likelihood function attains its maximum, so equivalently, the location at which the negative log likelihood attains its minimum (for this situation there is indeed a unique minimum). So each component of fixed is answering the question: "should this parameter be "fixed" in the optimization?"

If one of mu or sigma is being fixed - the fixed value is placed in the appropriate entry of the fixed vector (first entry for mu, second entry for sigma).

Note 0 when considered as (when converted to) a logical value is FALSE and any non-zero numeric value (including negative values) when considered as a logical value converts to TRUE Also recall that when used in a numerical context, FALSE converts to 0 and TRUE to 1

Consider the components fixed\[1\] and fixed\[2\] of fixed where fixed is equal the vector that the make.NegLogLik function is called with. The default value of fixed has been set to c(FALSE, FALSE) in the argument list.

If fixed\[1\] (corresponding to mu) is FALSE or zero, then that component of !fixed ("not fixed", note the ! indicating logical negation) will be TRUE, indicating that the value of mu is NOT fixed and so an optimal value of mu should be searched for, and similarly for fixed\[2\] corresponding to sigma. Note !fixed ("not fixed") is forced (converted, coerced) to a logical vector even if fixed had numeric entries.

If fixed\[1\] (corresponding to mu) is a non-zero value, then that component of !fixed ("not fixed") will be FALSE, indicating mu should be fixed (and as we will see) set equal to the value of fixed\[1\] that make.NegLogLik was called with, and similarly for fixed\[2\] corresponding to sigma.

So the bottom line is: to optimize over both mu and sigma, set fixed = c(0, 0) or fixed = c(FALSE, FALSE) which is the default value for fixed in make.NegLogLik

To fix mu at some value m and only search over sigma, set fixed = c(m, 0) or set fixed = c(m, FALSE), but note if you want to fix mu at 0 you can NOT do this exactly, since having fixed\[1\] equal 0 will "tell" NegLogLik to search over mu, so if you want to fix mu at 0 one has to "fool" this setup by having m be a very small number, for example 1.0e-14 So for the case where mu is being fixed, !fixed will be c(FALSE, TRUE) and the optimization will be done over only sigma.

And similarly with fixed\[2\] corresponding to sigma. If fixed is c(0, s) or c(FALSE, s) where s is a nonzero value for sigma (sigma = 0 is not admissible for a normal distribution), then !fixed will be c(TRUE, FALSE) and the minimization will be over only mu.

So let's see line by line how this works. Now I am adding explanatory comments to the make.NegLogLik function.

### make.NegLogLik from web-opt \[1\], Peng \[1\] with many explanatory comments added

    make.NegLogLik <- function(data, fixed = c(FALSE, FALSE)) {
        params <- fixed  # this defines params to be a vector of length 2

        function(p) {      #### ***  this is the function defined by make.NegLogLik *** ####

    #        When searching over mu and sigma, the argument p 
    #        will be a vector of length 2 containing values of mu and sigma
    #        at which the optimization routine "wants" to calculate a value.
    #        If one of mu or sigma has been fixed, then p will be a vector of
    #        of length 1 (a single value) for the parameter (mu or sigma) being
    #        searched over
           params[!fixed] <- p  
    #        note in the parent environment of the function being defined within make.NegLogLik,    
    #        which is the environment of make.NegLogLik, the variable params
    #        has been defined to be a vector of length 2 equal to fixed.
    #        This line (params[!fixed] <- p)  places new value(s) into the correct
    #        entry or entries of params where !fixed is TRUE 
    #        and is one reason for all the discussion above about fixed.  
     
    #        When this function of p (which is what is returned by make.NegLogLik) "looks for"
    #        params and fixed to redefine one or both of the entries of params, it "fetches" params and fixed from 
    #        its parent environment (the environment of make.NegLogLik) by lexical scoping, and similarly for 
    #        data when it is used below in computing the negative log likelihood.

    #        The other special part of this situation is that since make.NegLogLik is defining
    #        and returning this function, for example by the call

    #      nLL <- make.NegLogLik(data = normals, fixed = fixed.vector)

    #        where normals is the data vector we are fitting a normal distribution to, and
    #        fixed.vector is the user supplied value for fixed,
    #        then nLL is a "function closure" 
    #        (it is a function that was defined inside another function) 
    #        and so not only is it the function being defined
    #        here, it also retains the value of the R objects  params  and  fixed  and  data
    #        in its environment, and so has access to them. 
    #        If both parameters are being searched over then !fixed is c(TRUE, TRUE)
    #        and so params is simply set to p (and p will be a vector containing
    #        a value for mu and for sigma at which to evaluate the negative of the
    #        log likelihood). The optimization program that is doing the searching 
    #        will be calling this function multiple times in carrying out the search.
    #
    #        Now for the other "clever" part.  Suppose we were fixing mu = m and searching
    #        over sigma.  Then fixed will have the form c(m, 0) or c(m, FALSE) with
    #        m not 0, so then !fixed will be the logical vector c(FALSE, TRUE).
    #        Recall the first line of the make.NegLogLik function

    #     params <- fixed  

    #        This will set params to have the value c(m, FALSE)
    #        and the line inside this function

    #     params[!fixed] <- p  

    #        will only change params[2] (corresponding to sigma) to be equal p
    #        which will be a single value when only doing the search over sigma.
    #        The value of params[1] which was initially set to m WILL NEVER BE CHANGED,
    #        and will be preserved between calls by the optimization routine to nLL
    #        because nLL is a function closure.

    #        Similarly, if fixed was set to c(0, s) with s a nonzero value for sigma, then
    #        the search will be over mu, and sigma will be fixed to have the value s; that 
    #        is params[2] will be set to s, never changed, and preserved between calls to nLL,
    #        and params[1] (for mu) will be set to the value p provided by the optimization routine.

           mu <- params[1]
           sigma <- params[2]

    #        calculate the negative log likelihood

           a <- -0.5*length(data)*log(2*pi*sigma^2)
           b <- -0.5*sum((data-mu)^2) / (sigma^2)
           -(a + b)
    #        this is the negative of the log likelihood function for a normal distribution
    #        see for example the Wikipedia web site web-wiki [1] noted above
        }
     }

run the examples from the Roger Peng book / cited web page
----------------------------------------------------------

Now, to have this all in one place, I'll run optimization examples as in the Peng \[1\] and web-opt \[1\], with some commentary added. The make.NegLogLik function has already been defined and "compiled" above.

``` r
set.seed(1)  #  define some arbitrary but, importantly, known initialization for the random number
#               generator, so the calculation is reproducible
normals <- rnorm(100, mean = 1, sd = 2)
#               generates 100 (approximately) independent random samples
#               from a normal distribution with mean 1 and standard
#               deviation 2
#               The sample mean and sample standard deviation of the 100
#               values in the normals vector would be expected to be
#               in the general vicinity of 1 and 2, respectively, but since
#               this is a finite sample, the sample mean and sample standard
#               deviation will not in general be equal the mean and standard
#               deviation of the distribution the samples were from.

nLL <- make.NegLogLik(normals)   # use the default value fixed = c(FALSE, FALSE)
#                                  This creates the function which evaluates the 
#                                  negative log likelihhood function for 
#                                  searching over mu and sigma

#               print out the nLL function
nLL
```

    ## function(p) {
    ##                 params[!fixed] <- p
    ##                 mu <- params[1]
    ##                 sigma <- params[2]
    ##                 a <- -0.5*length(data)*log(2*pi*sigma^2)
    ##                 b <- -0.5*sum((data-mu)^2) / (sigma^2)
    ##                 -(a + b)
    ##         }
    ## <bytecode: 0x0000000014eab300>
    ## <environment: 0x0000000014d47a30>

``` r
ls(environment(nLL))   # list what is in the environment
```

    ## [1] "data"   "fixed"  "params"

``` r
#                        of nLL, showing that data, fixed and params are preserved, and so available to nLL


#               Now do the search for the optimal mu and sigma for the data in normals
#               Use R's optim function to do the search and "give it"
#               the starting values mu = 0 and sigma = 1 and print out the values it found
optim(c(mu = 0, sigma = 1), nLL)$par   #  the call to optim in the book and web site
```

    ##       mu    sigma 
    ## 1.218239 1.787343

``` r
optim(c(mu = 0, sigma = 1), nLL, method = "CG")$par  
```

    ##       mu    sigma 
    ## 1.217775 1.787395

``` r
#               Note I chose to use the Conjugate Gradient method rather than the optim's default
#               minimization method, see the R help for the optim function.
#               This gives a bit more accurate result for this calculation, as does having  
#               method = "BFGS" (the Broyden-Fletcher-Goldfarb-Shanno method).
#               The extra accuracy seen here may not be relevant for some practical problems where
#               random error or  measurement inaccuracy make more than a few significant digits 
#               rather meaningless, but here the extra accuracy is nice to have when checking against
#               a known exact value.


#               The mu determined by optim should be virtually the same as the mean of the normals vector
#               (subject to the accuracy of the optimization routine)
#               and the value of sigma^2 determined by optim should be virtually the same as 
#               (the sample variance of the normals vector) * (length(normals) - 1) / length(normals)
#               Note the maximum likelihood estimate of sigma^2 is not exactly the same as the sample variance, 
#               but approaches it as the size of the sample becomes large.
#               The statistical terminology for the sentence above is: the estimate is biased but consistent.

mean(normals)  #  what optim should find for mu, subject to the accuracy of the numerical optimization 
```

    ## [1] 1.217775

``` r
maxlik_var <- var(normals) * (length(normals) - 1) / length(normals)
sqrt(maxlik_var)  # what optim should find for sigma, subject to the accuracy of the numerical optimization 
```

    ## [1] 1.787394

``` r
#
#
#               Now do an example with sigma fixed and searching for the optimal value of mu for
#               this fixed value of sigma.  For a normal distribution it turns out that the
#               maximum likelihood estimate for mu will be the mean of the data even if sigma is 
#               fixed at some value not equal the standard deviation of the data 
# 
mean(normals)     # what the answer should be              
```

    ## [1] 1.217775

``` r
#           

#               Do the calculation using the appropriate nLL for this case and R's optimization function
#               optimize which searches for a minimum of a function over 1 parameter.
#               One needs to call optimize with the function to be minimized and an interval to search over

nLL <- make.NegLogLik(data = normals, fixed = c(FALSE, 2))  # fix sigma to be 2, calculate mu
optimize(nLL, c(-1, 3), tol = 1.0e-7)$minimum
```

    ## [1] 1.217775

``` r
#               I have imposed a smaller tolerance than the default in order to
#               show the calculated result using nLL really agrees with the "true value"



#
#
#               Now fix mu = m = 1.0 and search for sigma. Note this choice of m is NOT the mean of normals.
#               Using some calculus (see the Wikipedia page cited above)
#               one can show that the value of sigma in this case (mu set to m = 1.0) should be

m <- 1.0
max_lik_variance_for_this_m <- sum((normals - m)^2) / length(normals)  # maximum likelihood estimate for sigma
#                                                                        given that mu has been fixed at the value m
max_lik_sigma_for_this_m <- sqrt(max_lik_variance_for_this_m)
max_lik_sigma_for_this_m 
```

    ## [1] 1.800612

``` r
#               Do the calculation using the appropriate nLL for this case and R's optimization function
#               optimize which searches for a minimum of a function over 1 parameter.
#               One needs to call optimize with the function to be minimized and an interval to search over

nLL <- make.NegLogLik(data = normals, fixed = c(m, FALSE))  # fix mu to be m (here 1), calculate sigma
optimize(nLL, c(1e-6, 10), tol = 1.0e-7)$minimum
```

    ## [1] 1.800612

``` r
#               I have imposed a smaller tolerance than the default in order to
#               show the calculated result using nLL really agrees with the "true value"
```

Hope this explanation clarifies how make.NegLogLik works.

Appendix: Some Comments on Function Closure
-------------------------------------------

This appendix is taken and considerably modified from a pinned post of mine "Some examples of why lexical scoping is useful and What is a Function Closure" in the Week 2 Discussion Forum for the R Programming Course in the Johns Hopkins Data Science Specialization on Coursera: <https://www.coursera.org/learn/r-programming/discussions/weeks/2/threads/sKnK4U0xStmpyuFNMerZFw> One needs to be registered with Coursera to have access to this web page (this Appendix does not depend on your reading the pinned post). **see the legal notices pertaining to this Appendix at the bottom of this article**

The way R (and some other programming languages) incorporate lexical scoping (it is being assumed that the reader has some acquaintance with the basic idea of "finding values for" locally undefined variables using lexical scoping) allows one to use a function (call it H) to create and return an object (a **function closure**) (call it F) that is a function (the case I'll describe further here), or is a list of functions. In this setting, the R object F preserves variables (R objects) that were present in the environment of H at the time execution of H ended (at the time H exited returning F). F is then a “self contained” object that can contain variables within it that are available to F when F is used (for example the nLL function / R object discussed in detail above). These variables can also, with the right set up, be “fetched” (often done by a function with “get” in its name) or changed/updated (often by a function with “set” in its name). The big advantage is that these variables can be initialized as parameters that are preserved between uses of F, and are "safe" from unintentional modification by other functions. This is the appropriate setting for use of the superassignment operator &lt;&lt;- that for example would allow for the function F to change the value of a variable in the environment of F (i.e., variables that were in the environment of H when H finished executing; this includes the arguments H was called with).

Leonard Greski's excellent article "Demystifying makeVector()" <https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md> explains how function closure works (why arguments and local variables are "preserved" when a function that defines and returns one or more functions finishes execution, and how the superassignment operator works in this context). The functions under discussion in this article are listed here <https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md#appendix-b-cachemeanr> The diagram and discussion in this section <https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md#whats-going-on-in-makevector> goes into detail on the workings of lexical scoping and function closure. Note his article does not use the term "function closure" but that is what this section is explaining. This is an example of the case where H returns a list of functions including "get" and "set" type functions.

Here is an example which is a modification of a lexical scoping example from Peng \[1\] and the web-opt \[1\] site (located before the "Application: Optimization" section). I've added the assignment of z and the cat (print) statements (and note cat needs to be "told" to start a new line (when one wants it to) which is what "\\n" will do. An advantage of cat is that it will print multiple things from one call.

``` r
make.power <- function(n) {
    z <- 25
    pow <- function(x) {
    cat("z = ", z, "  # returned by cat", "\n") # "\n" is newline
    cat("n = ", n, "  # returned by cat", "\n")
    x^n
    }
 pow
 }
```

When, for example, make.power is called with n = 2, it assigns 25 to z; then defines the function pow as a function of the argument x, which will return x^2 here since in this example n was set to 2 in the call to make.power. By lexical scoping, when R encounters the statement x^n inside the function pow as the function pow is being defined, R, upon "not seeing" n defined inside pow, will "look for" a value of n in the parent environment of pow which is make.power since that is where pow is being defined (and in this example make.power is being called with n = 2).

The additional "magic" in this situation is that since make.power defines and returns a function, the object returned by make.power is a **function closure**, which means it not only contains the pow function, it also contains (has access to) the argument n make.power was called with, as well as the local variable z in the environment of make.power that had a value when make.power finished executing. Note while make.power was executing, it was the parent environment of the pow function. This behavior is discussed at length in Len Greski's Demystifying makeVector article referenced above. If there were more arguments and or more local variables defined in the environment of the function H then all of them would be preserved/contained in the environment of the returned object F.

The cat statements demonstrate that z and n are indeed "preserved":

``` r
square.function.closure <- make.power(n = 2)
square.function.closure(x = 7)  # should return 7^2
```

    ## z =  25   # returned by cat 
    ## n =  2   # returned by cat

    ## [1] 49

``` r
square.function.closure(x = 25) # should return 25^2
```

    ## z =  25   # returned by cat 
    ## n =  2   # returned by cat

    ## [1] 625

``` r
# Another example.
# in the Global Environment (the R workspace)
# set values for z and n (that are different from the values
# of the z and n variables within the square.function.closure environment)

z <- 12345
n <- 4
square.function.closure(x = 7)
```

    ## z =  25   # returned by cat 
    ## n =  2   # returned by cat

    ## [1] 49

The crucial point here is that n and z are variables within the square.function.closure environment and so their values there are "safe" from unintentional external modification

This is also an example of having variables (R objects) located in different R environments while having the same name but different values (and "storage locations"). While this is perfectly "legal in R", one should still be judicious about naming variables in order to avoid confusion which can lead to hard to find bugs

One more example. Note that local variables **inside the function being returned** (inside F in the notation
above) as opposed to variables in the parent environment of the function being returned (the parent environment of F at the time the function F was being defined, which is the environment of H in the notation above) are NOT preserved. This is shown in this example, where vloc is a local variable inside F, and the R **exists**
function returns TRUE or FALSE depending on whether its argument (the name of an R object as a character string) exists in the environment where exists was called (unless some other environment is specified).

``` r
make.power <- function(n) {
    z <- 25
    pow <- function(x) {
        print(exists("vloc"))
        cat("z = ", z, "  # returned by cat", "\n") # "\n" is newline
        cat("n = ", n, "  # returned by cat", "\n")
        vloc <- 1234
# We are going to test if vloc is preserved and so still available when
# the function (function closure) defined by calling make.power is
# called a second time (as stated above, we will see that it is not - the 
# print(exists("vloc")) statement above will again return FALSE).
        print(exists("vloc"))
        x^n
    }
    pow
}

square.function.closure <- make.power(n = 2)
square.function.closure(x = 7)  # should return 7^2
```

    ## [1] FALSE
    ## z =  25   # returned by cat 
    ## n =  2   # returned by cat 
    ## [1] TRUE

    ## [1] 49

``` r
square.function.closure(x = 8)  # second call 
```

    ## [1] FALSE
    ## z =  25   # returned by cat 
    ## n =  2   # returned by cat 
    ## [1] TRUE

    ## [1] 64

Uses for Lexical Scoping
------------------------

This can be very convenient, for example in some numerical procedures such as optimization where there are one or more parameters (denoted by the vector P) in a function f for which one wants to find the value of P at which f or some function depending on f attains a minimum (or maximum) value. It is helpful for the optimization procedure to be able to update the values of the parameter(s) P internally without needing “external” instructions from the programmer after the optimization procedure is started. This is the setting of the example that is the main subject of this article. Uses of lexical scoping are also noted in Darren Wilkinson's blog post “Lexical scope and function closures in R” (read down through “Function closures for scientific computing”). <https://darrenjw.wordpress.com/2011/11/23/lexical-scope-and-function-closures-in-r/>

The article “Lexical Scope and Statistical Computing” by Robert Gentleman and Ross Ihaka <https://www.stat.auckland.ac.nz/~ihaka/downloads/lexical.pdf> mentions the type of examples above and also gives a conceptual example of constructing multiple copies of a random number generator. Since in general random number generators depend on an initial seed value, having multiple independent ones requires use of some type of programming feature, which lexical scoping/function closure makes straightforward.

The book “Advanced R” by Hadley Wickham <http://adv-r.had.co.nz/> has in depth discussion of many topics including lexical scoping <http://adv-r.had.co.nz/Functions.html#lexical-scoping>, and function closures <http://adv-r.had.co.nz/Functional-programming.html#closures> and gives an example where one can keep an internal count of how many times a function has been called (scroll down to the section “Mutable state”). One might, for example, want this available when doing optimization, where one usually will set an upper bound on the number of calls allowed to the function being minimized for an optimization method to reach convergence (i.e., satisfy some convergence criterion), otherwise one exits with a warning or error message, in order to avoid running forever when the method is failing to find a minimum (or maximum), and also just to return the information on how many function calls it took when the method succeeded. There is also an example in "Advanced R" of a maximum likelihood estimation <http://adv-r.had.co.nz/Functionals.html#functionals-math>

Attributions and legal notices
------------------------------

The web site <https://github.com/DataScienceSpecialization/courses/blob/master/02_RProgramming/Scoping/index.md#application-optimization>
cited above which contains the "Application: Optimization" R code discussed above is a subsection of the <https://github.com/DataScienceSpecialization/courses> web page. The contributors for this web site are: Brian Caffo, Jeff Leek, Roger Peng, Nick Carchedi, and Sean Cross

This web site contains course materials for the Johns Hopkins Data Science Specialization on Coursera <https://www.coursera.org/specialization/jhudatascience/1> The License listing for the <https://github.com/DataScienceSpecialization/courses> web site is: "These course materials are available under the Creative Commons Attribution NonCommercial ShareAlike (CC-NC-SA) license (<http://www.tldrlegal.com/l/CC-NC-SA>)." The link in the line above no longer works.

The apparent current version of this license is the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0) license available at <https://creativecommons.org/licenses/by-nc-sa/4.0/> and the full legal version is at <https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode>

Any licensing that applies for <https://github.com/DataScienceSpecialization/courses> also applies to this work (in order to preserve the rights of the authors of that web site). The Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0) license whose links are above applies for this article (assuming this license is in effect equivalent to the License listed in the <https://github.com/DataScienceSpecialization/courses> web page: any restrictions that apply for that web site apply to this work, and no further restrictions are intended).

Material in the Appendix above on Function Closure and Lexical Scoping is taken/modified from a pinned post of mine "Some examples of why lexical scoping is useful and What is a Function Closure" in the Week 2 Discussion Forum for the R Programming Course in the Johns Hopkins Data Science Specialization on Coursera. As such Coursera and Coursera authorized Partners retain additional rights to that material as described in their "Terms of Use" <https://www.coursera.org/about/terms>

Note the reader should not infer any endorsement or recommendation or approval for the material in this article from any of the sources or persons cited above or any other entities mentioned in this article.
