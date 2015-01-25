## The two functions below allow for faster inverse matrix usage by caching the inverse of the matrix.
## makeCacheMatrix creates an object based on a matrix argument.
## cacheSolve returns the inverse of the original matrix argument (via the makeCacheMatrix object)
## in the most efficient way available (caching the value if already calculated or calculating the inverse for teh first time)

## This function is the constructor function taking as an argument a defined matrix "x"
## Initially the inverse "i" is set to NULL.
## Includes helper functions for the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL  #initialize the inverse matrix denoted by "i" to NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x  #returns the matrix
    setinverse <- function(inverse) i <<- inverse  #stores the inverse matrix to "i"
    getinverse <- function() i  #returns the inverse matrix "i"
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function either returns the cached inverse of the argument type makeCacheMatrix or calculates it 
## before returning the inverse matrix.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()  #retrieves the inverse matrix
    
    #checks to see if the inverse was already calculated. If it was, then return it without calculating it again.
    if(!is.null(i)){
        message("getting cached data")
        return (i)
    }
    
    #otherwise calculate the inverse matrix, store it, and return it
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i

}
