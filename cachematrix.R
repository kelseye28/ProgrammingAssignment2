## These two functions allow you to cache the inverse of a matrix in a different
## environment and retrieve it. This can save time on long calculations.

## this function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## this function computes the inverse of the matrix returned by makeCacheMatrix
## if the inverse has already been calculated, then it returns the matrix from 
## the cache instead

cacheSolve <- function(x, ...) {
        
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
