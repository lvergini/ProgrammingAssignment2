## This script defines two functions:
## - makeCacheMatrix: creates a special matrix object that can cache its inverse.
## - cacheSolve: computes the inverse of the special matrix, using a cached value if available.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # This variable will hold the cached inverse
    ## set: replaces the current matrix and resets the cached inverse
    set <- function(y) {
        x <<- y        # assign new matrix to x in parent environment
        inv <<- NULL   # reset the cached inverse
    }
    
    ## get: returns the current matrix
    get <- function() x
    
    ## setinverse: stores the inverse in the cache
    setinverse <- function(inverse) inv <<- inverse
    
    ## getinverse: retrieves the cached inverse, if it exists
    getinverse <- function() inv
    
    ## return a list of the above four functions to interact with the object
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed,
## then it retrieves the inverse from the cache instead of computing it again.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()  # attempt to retrieve the cached inverse
    
    ## if the inverse is already cached, return it with a message
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## otherwise, compute the inverse
    data <- x$get()            # get the matrix
    inv <- solve(data, ...)    # compute the inverse using solve()
    x$setinverse(inv)          # cache the computed inverse
    inv                        # return the inverse
}