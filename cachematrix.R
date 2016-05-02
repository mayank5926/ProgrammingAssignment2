## COursera: Introduction to R programming
## Assignment: Programming Assignment 2: Lexical Scoping

## Assignment description: Matrix inversion is usually a costly computation and
## there may be some benefit to caching the inverse of a matrix rather than
## compute it repeatedly.

## Assignment is to write a pair of functions that cache the inverse of a
## matrix. Functions below are created on the pattern of functions makeVector
## and cachemean explained in the assignment.

## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse. 

makeCacheMatrix <- function (x = matrix ()) {
        ## Initialize inverse matrix
        inverted <- NULL
        
        ## Function to get matrix
        set <- function (y) {
                x <<- y
                inverted <<- NULL
        }
        
        ## Function to get matrix
        get <- function () x
        
        ## Function to set inverse of the matrix
        setInv <- function (inv) inverted <<- inv
        
        ## Function to get inverse of the matrix
        getInv <- function () inverted
        
        ## Return list of functions for use
        list (
                set = set,
                get = get,
                setInv = setInv,
                getInv = getInv
        )
}

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function (x, ...) {
        ## Try to get inverse of matrix from cache
        inv <- x$getInv ()
        
        ## Check if inverse value is not yet initialized
        if (!is.null (inv)) {
                ## If initialized, return value from cache
                message ("getting cached data")
                return (inv)
        }
        
        ## If inverse of matrix is not yet computed, compute now.
        ## Get the value of matrix
        data <- x$get ()
        
        ## Compute inverse of the matrix
        inv <- solve(data,...)
        
        ## Save the inverse of matrix in cache for later use
        x$setInv(inv)
        
        ## Return inverse of matrix
        inv
}
