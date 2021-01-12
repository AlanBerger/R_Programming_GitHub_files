function closure in terms of environments discussion 11 Jan 2020.Rmd
--------------------------------------------------------------------

### Alan E. Berger

### available at <https://github.com/AlanBerger/R_Programming_GitHub_files>

I'm going to use as an example a modification of a lexical scoping example in Roger Peng's "R Programming for Data Science", LeanPub, Last updated on 2020-09-03; and material on environments in Garrett Grolemund's "Hands-On Programming with R", <https://rstudio-education.github.io/hopr/> and in "Advanced R" by Hadley Wickham, <http://adv-r.had.co.nz/>, and in "Environments in R: an exploration" by Pier Lorenzo Paracchini, 21.05.2017, <https://rstudio-pubs-static.s3.amazonaws.com/278710_bb8897865caf43c6a39757278547b1f4.html>

When a "normal" function (that just does some calculation and returns a "value" (R object)) is called (executed) R sets up an execution environment where the arguments the function was called with and any local variables of the function have values. When the function finishes, this execution environment and the local variables "go away". If a function H defines another function Z in the body of H, then the execution environment Ex of H is no longer temporary, and it (Ex) becomes the parent environment of Z (since that is where Z was defined).
So if we do `F <- H(suitable values for the arguments of H)` and the last line in the body of H was `return(Z)` then F will be a **function closure** which will be a copy of the function Z **and the parent environment of F will be Ex**. In the case that Z is **a list of functions that were defined in H**, then **the parent environment of each of the functions f in the list will be Ex**.

The consequence (as explained in detail in Leonard Greski's
"Demystifying makeVector" <https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md>
is that the values H was called with, and the values of any local variables in the body of H are preserved in Ex. Hence they can be "found and used" by lexical scoping from within the function F (or if Z was a list of functions, these values can be accessed by each of the functions f in the list Z and in its returned copy F).

Consider for example

    rm(list = ls())  # remove any "left over" objects in the R workspace

    make.power <- function(n) {
        z <- 25
        pow <- function(x) {
            cat("z = ", z, "\n") # "\n" is newline
            cat("n = ", n, "\n")
            x^n
        }
        pow
    }

This function defines a local variable z to be 25 and defines and returns the pow function. It also prints out the values of z and n

The environment of make.power (more precisely, the execution environment of makepower that R creates for a given call to it) will become the parent environment of the function pow created by make.power **and also of the copy of pow returned by that call to make.power**

Now let's call make.power and explore:

    square.function.closure <- make.power(n = 2)

    # let's confirm we are working in the usual R workspace
    # i.e., the Global Environment

    environment()  # get current active environment
    <environment: R_GlobalEnv>
    # as expected

    # Now let's find the enclosing/parent environment
    # of the square.function.closure function

    environment(square.function.closure)
    <environment: 0x000000001540f588>

    # This is the parent environment of square.function.closure 
    # Note the R object square.function.closure is located in the
    # R workspace (the global environment)

    ls()
    [1] "make.power"              "square.function.closure"

But importantly, unlike "usual" functions that were defined in the workspace, square.function.closure has a separate enclosing (parent) environment where additional variables (R objects) (here n and z) are accessible, and are safe from being changed or deleted (unless by explicit action by the programmer).

Let's explore the enclosing (parent) environment of square.function.closure It will be convenient to have an R object of class *environment* that we can use to refer to it.

    env.sq.fcn.cl <- 
        environment(square.function.closure)
    env.sq.fcn.cl
    <environment: 0x000000001540f588>   
    class(env.sq.fcn.cl)
    [1] "environment"

    # this is the environment R created to run 
    # make.power for this call to it (which happens
    # to have been with n = 2)

    # so square.function.closure is not just a 
    # simple function that squares the value of  
    # its argument, it also has a specific to it
    # parent (also called enclosing) environment 
    # that holds (in this case) the value of n (here 2) 
    # and the value of z (here 25)

    # We can actually list them:
    ls(env.sq.fcn.cl)
    [1] "n"   "pow" "z"  

    # use the function
    square.function.closure(12)
    z =  25 
    n =  2 
    [1] 144
    # recall this function prints out z and n 

    # find the value of one of the variables
    # in the closure

    env.sq.fcn.cl$z
    [1] 25

    env.sq.fcn.cl$n
    [1] 2

    z <- 12345  # this z is in the R workspace
    #             i.e., in the Global Environment
            
    env.sq.fcn.cl$z
    [1] 25

    # so the value of z in the parent 
    # environment of
    # square.function.closure is not affected by
    # having another variable (R object)
    # called z located in another environment

    # note (see Leonard Greski's article) the 
    # makeVector function returns get and set 
    # functions that allow the user to directly
    # fetch (get) and set the value of 
    # certain variables available to the functions in the list 
    # returned by makeVector
