# R Programming Assignment 2: Lexical Scoping
#
# Contains two functions written to be graded by peers. These functions support
# a type of matrix inversion caching following (rather precisely) the example
# given for caching the mean of a vector. [Note: a lazy-evaluation "mean" function
# would seem to make more sense but maybe that's not R-esque]
#
# makeCacheMatrix: turn normal matrix in one with functions bound to it
# cacheSolve: return inverse of matrix and cache if not already stored
# A2unitTest: bonus function - unit test for the two functions above
#
# Commenting follows Google's R Style Guide
# see: https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml#functiondocumentation


makeCacheMatrix <- function(A = matrix()) {
    # This function creates a special "matrix" object that can cache its inverse.
    #
    # Args:
    #   A: A square, invertable matrix (by contract, not tested)
    #
    # Returns:
    #   A list with the following methods, er, functions:
    #       set: resets the matrix to a new matrix
    #       get: gets the stored matrix
    #       setinverse: stores
    #       getinverse: 
    
    # initialize 
    invA <- NULL  # I like "mu" which is more descriptive than "m"

    set <- function(B) {
        A <<- B        # gA = "global A" or at least "outer scope A"
        invA <<- NULL
    }
    get <- function() A  # shorthand to avoid useless (?) curly braces
    setinverse <- function(B) {
        invA <<- B  # use curly braces this time. Set mu in global scope, er, environment
    }
    getinverse <- function() invA  # shorthand again
    
    # last thing done in a function is returned. Here is the promised list.
    list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)
}


cacheSolve <- function(A, ...) {
    # This function computes the inverse of the special "matrix" returned by 
    # makeCacheMatrix above. If the inverse has already been calculated 
    # (and the matrix has not changed), then the cachesolve should retrieve the 
    # inverse from the cache.
    #
    # Args:
    #   A: A "cache matrix" created above (following normal Ax=b nomenclature)
    #
    # Returns:
    #   A list with the following methods, er, functions:
    #       set: resets the matrix to a new matrix
    #       get: gets the stored matrix
    #       setmean: sets the mean (externally!) to cache it for future references
    #       getmean: gets the set (or NULL) mean
    
    invA <- A$getinverse()
    if (!is.null(invA)) {
        return(invA)
    }
    invA<-solve(A$get())
    A$setinverse(invA)
    invA
}

## Simply call A2unitTest() and the return shoud be 
## [1] TRUE
A2unitTest <- function() {
    # Unit test for the two functions above.
    # The directly calculated inverse should match the cached inverse
    # The time to retrieve the inverse the 2nd time should be significantly 
    # less than the 1st time

    A <- matrix(c(1,2,3,6,5,4,15,-3,12),3,3)
    iA <- solve(A)
    B <- makeCacheMatrix(A)
    iB <- cacheSolve(B)
    pass1 <- identical(iA,iB)
    
    # run this test 10 times
    delta1sum <- 0
    delta2sum <- 0
    for (n in 1:10) {
        A <- matrix(rnorm(513*513),513,513)  # non powers of 2 take longer to solve
        B <- makeCacheMatrix(A)
        
        t1 <- proc.time()
        iB1 <-  cacheSolve(B)
        delta1 <- proc.time() - t1
        delta1sum <- delta1sum + delta1[["elapsed"]]
        # duplicate exact sequence
        t1 <- proc.time()
        iB1 <-  cacheSolve(B)
        delta2 <- proc.time() - t1
        delta2sum <- delta2sum + delta2[["elapsed"]]
    }
    # first time solve should take longer than cached solve
    pass2 <- delta1sum > delta2sum
    pass1 && pass2   # return logical and of the two tests
}
