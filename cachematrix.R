## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a matrix wrapper type which caches computation of the inverse.

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y) {
                     x <<- y
                     inv <<- NULL
           }

           get <- function() x
           setinv <- function(invm) inv <<- invm
           getinv <- function() inv
           list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
}


## Returns the inverse of a matrix for a matrix-wrapper returned by makeCacheMatrix
## Computes the inverse the first time it is asked, caches the inverse which it returns is asked again.
## Changing the matrix resets the cache

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
