reply to where function closure variables are 8 Jan 2020.Rmd
------------------------------------------------------------

### Alan E. Berger

### available at <https://github.com/AlanBerger/R_Programming_GitHub_files>

I'm going to use as an example a modification of a lexical scoping example in Roger Peng's "R Programming for Data Science", LeanPub, Last updated on 2020-09-03; and material on environments in Garrett Grolemund's "Hands-On Programming with R", <https://rstudio-education.github.io/hopr/> and in "Advanced R" by Hadley Wickham, <http://adv-r.had.co.nz/>

When a "normal" function (that just does some calculation and returns a "value" (R object)) is called (executed) R sets up an execution environment where the arguments the function was called with and any local variables of the function have values. When the function finishes, this execution environment and the local variables "go away". If a function H defines another function (or a list of functions) Z in the body of H, then the execution environment Ex of H is no longer temporary, and it (Ex) becomes the parent environment of Z (since that is where Z was defined). The consequence (as explained in Leonard Greski's "Demystifying makeVector" <https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md> is that the values H was called with, and the values of any local variables in the body of H are preserved in Ex. Hence they can be "found and used" by lexical scoping from within the function Z.

Consider for example

    make.power <- function(n) {
        z <- 25
        pow <- function(x) {
            cat("z = ", z, "\n") # "\n" is newline
            cat("n = ", n, "\n")
            x^n
        }
        pow
    }

This function defines a local variable z to be 25 and defines and returns the pow function.

The environment of make.power (more precisely, the execution environment of makepower that R creates for a given call to it) will become the parent environment of the value of pow returned by that call to make.power Now let's call make.power and explore:

    square.function.closure <- make.power(n = 2)

    # Now let's find its 
    # enclosing/parent environment
    environment(square.function.closure)
    <environment: 0x000000001540f588>

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
    # that holds (here) the value of n (here 2) 
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
    # certain variables in the function closure
    # returned by makeVector
