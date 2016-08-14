# These functions allow the cache of a matrix inverst to be saved to to
# lighten the processing load by preventing calculating the inverse repeatedly

makeCacheMatrix <- function(x = matrix()) {
    # This function defines a list the has built in functions for handling the 
    # inverse of the matrix
    
    inv <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    # Pull from cache is possible, otherwise calculate, cache, and return
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    return(inv)
}
