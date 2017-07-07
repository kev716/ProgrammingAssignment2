## These functions compute the inverser of a matrix and store it in a cache

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    cinv <- NULL
    set <- function(y) {
        x <<- y
        cinv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) cinv <<- inv
    getinv <- function() cinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function checks if the inverse of the matrix has been cached, and computes it if it 
## hasn't

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
