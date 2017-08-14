#
## makeCacheMatrix() and cacheSolve()
#
# Since computation of matrix inverses can be an expensive operation in terms of cpu time
# it is often required to 'remember' the inverse of a given matrix for use several times
# in a procedure.
# The following pair of functions allow one to construct an object that 'caches' matrix (makeCacheMatrix)
# and calculate and remember the inverse (cacheSolve) 
# They use lexical scoping in R to 'remember' the matrix and its inverse
#
# Example usage:
#   source("cachematrix.R")
#   X <- makeCacheMatrix(matrix(c(4,9,11,2),2,2))
#   Xinv <- cacheSolve(X)
#   Xinv <- cacheSolve(X,tol=0.001)  



### makeCacheMatrix(x)
#
# Builds a 'cached' matrix
#
# (optional) argument x: matrix
#
# returns an object of class c("list","CacheMatrix")
# The list of "set","get","setsolution" and "getsolution" functions:
# set(y): y is a matrix, function 'remembers' y and sets its cached solution to NULL
# get(): retrieves the values of the matrix
# setsolution(x_solv): 'remembers' the solution (inverse) to the matrix
# getsolution(): retrieves the cached solution (inverse)

makeCacheMatrix <- function(x = matrix()) {
    solve_of_x <- NULL
    set <- function(y) {
        x <<- y
        solve_of_x <<- NULL
    }
    get <- function() x
    setsolution <- function(x_solv) solve_of_x <<- x_solv
    getsolution <- function() {
        solve_of_x
    }
    rtv_x <- list(set = set, get = get,
                    setsolution = setsolution,
                    getsolution = getsolution)
    class(rtv_x) <- c(class(rtv_x),"CacheMatrix")
    rtv_x
}


## cacheSolve(x,...)
#
# argument x: a 'CacheMatrix' object (created by makeCacheMatrix)
# optional arguments: arguments to 'solve(a,b,...)',
#                            if b is given it must be an identity matrix, dimensions of x above
# 
# returns the inverse of x, if it has already computed it, it returns the cached value.
# 
# Note: it generates a message 'Returning cached inverse of x' if it is using the cached value
#       suppressMessages(solveCache(x)) can be used to avoid console messages

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    if (!inherits(x,"CacheMatrix")) stop("Argument x is not a 'CacheMatrix'. Use makeCacheMatrix().")
    args_solve <- list(...)
    exists_arg_b <- "b" %in% names(args_solve)
    if (exists_arg_b) {
        if (!identical(args_solve$b,diag(nrow=dim(x$get())[1]))) {
            stop("Cache Solve is designed only for inverses, if argument 'b' is present, it must be an identity matrix of dimensions equal to argument 'x'")
        }
    }

    solv_of_x <- x$getsolution()
    if(!is.null(solv_of_x)) {
        message("Returning cached inverse of x")
        return(solv_of_x)
    }
    x_matrix <- x$get()
    if (is.null(x_matrix)) return(x_matrix)  # do not attempt to solve a null matrix, but do not fail
    solv_of_x <- solve(x_matrix, ...)
    x$setsolution(solv_of_x)
    solv_of_x
}
