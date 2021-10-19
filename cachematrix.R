## This script contains a pair of functions that cach the inverse of a matrix. It
## is written as part of the coursera's R programming course.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## Clears mat variable.
        mat <- NULL
        ## Defines the setter function.
        set <- function(y) {
            x <<- y
            mat <<- NULL
        }
        ## Defines the getter function.
        get <- function() x
        ## Defines the setter for the inverse function.
        setinverse <- function(inverse) mat <<- inverse
        ## Defines the getter for the inverse function.
        getinverse <- function() mat
        ##Stores the values in cache as list for easy retrieval.
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'.
    mat <- x$getinverse()
    ## If the inverse matrix is available in cache, it is retrieved.
    if(!is.null(mat)) {
        message("Getting cached data...")
        return(mat)
    }
    data <- x$get()
    ## In case it was not available, it is calculated and stored.
    mat <- solve(data, ...)
    x$setinverse(mat)
    ## Print the matrix.
    mat
}
