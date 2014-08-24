## This pair of functions allows you to compute the inverse of a matrix
## and cache the result. If the same inverse is calculated again in the future
## the cached result will be provided instead of re-computing it.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated (and
## the matrix has not changed), then `cacheSolve` retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

# Example code to test functionality and speed
#
# width <- 200
# matrix <- matrix(floor(rexp(width^2, rate=.1)), ncol=width, nrow=width)
# microbenchmark(inverted <- solve(matrix)) # 17.78 milliseconds
# 
# # Using cache
# # Create the vector object
# special.object <- makeCacheMatrix()
# special.object$set(matrix)
# special.object$get()
# microbenchmark(cacheSolve(special.object)) # 115 microseconds
# special.object$getinverse()






