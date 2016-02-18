## Computing an inverse of a square matrix, and caching the inverse of a matrix.  If the matrix   
## has not changed and the inverse has already been calculated, retrieve the inverse from the cache.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    cachedinverse <- NULL
    set <- function(y) {
        x <<- y
        cachedinverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) cachedinverse <<- inverse
    getinverse <- function() cachedinverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of a square "matrix" created by the makeCacheMatrix function.
## If the matrix has not changed and the inverse has already been calculated, return the inverse
## from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return (inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
