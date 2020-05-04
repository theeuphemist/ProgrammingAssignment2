## Abstract: In this chunk of code, my functions, makeCacheMatrix and cacheSolve,
#solve and return the inverse of a matrix.
##If the user commands to compute a previously computed matrix, then
#the result is retrieved from the cache created by the code.


## Part A: The makeCacheMatrix function creates a type of 'matrix' object. 
#This object can 'cache' its own inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x 
    setinv <-function(inverse) 
        inv <<- inverse
    getinv <- function() inv
    list( set = set,
          get = get,
          setinv = setinv,
          getinv = getinv)
}


## The cacheSolve function calculates the inverse of the 'matrix' returned by makeCacheMatrix.
#If the inverse is computed beforehand, then cacheSolve retrieves the results from the 'cache'.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        print("Hold on, fetching cached data.")
        return(inv)
    }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
    }
