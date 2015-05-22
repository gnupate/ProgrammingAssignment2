## makeCacheMatrix - creates a new matrix that can keep cached copies of it's inverse
## cacheSolve - finds the inverse of a CacheMatrix, checking for a cached inverse first
## solution by Pat Eyler, based on makeVector and cachemean from assignment


## makeCacheMatrix creates a new version of a matrix with set, get, setinverse, 
## and getinverse functions
## if given a matrix as it's argument, it copies the values in it to the new matrix
##
## the set function will overwrite the values in the CacheMatrix with the matrix provided
##
## the get function will return the matrix
##
## the setinverse function will set the cached inverse as whatever argument is provided
##
## the get inverse function will return whatever value is currently cached

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ## build the set, get, setinverse, and getinverse functions
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solution) i <<- solution
    getinverse <- function() i
    # build a list of the 4 functions and return it
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve will check to see if there is a cached copy of 
## the inverse of the matrix and return it if available.
## Otherwise it will calculate the inverse, cache it, and return it

cacheSolve <- function(x, ...) {
    ## first use getinverse to see if there's a cached version
    i <- x$getinverse()
    if(!is.null(i)) { ## if there's a cached version, return it
        return(i)
    } #otherwise find the inverse and cache it
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
