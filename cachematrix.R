# This pair of functions allows you to compute the inverse of a matrix
# and cache the result. If the same inverse is calculated again in the future
# the cached result will be provided instead of re-computing it.

makeCacheMatrix <- function(x = matrix()) {
    # This function creates a special "matrix" object that can cache its
    # inverse.
    #
    # Args:
    #  x: Initializes the value of x
    
    # Initialize the variable for storing the inverse
    m <- NULL
    set <- function(y) {
        # Store the data in the object
        x <<- y
        # Reset the mean to NULL when x changes
        m <<- NULL
    }
    # Return the data
    get <- function() x
    # Store the inverse in m
    setinverse <- function(inverse) m <<- inverse
    # Get the inverse
    getinverse <- function() m
    # Return everything
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    # This function computes the inverse of the special "matrix" returned by 
    # `makeCacheMatrix` above. If the inverse has already been calculated (and
    # the matrix has not changed), then `cacheSolve` retrieves the 
    # inverse from the cache.
    #
    # Args:
    #  x: A special object created using makeCacheMatrix
    #
    # Returns:
    #  The inverse of the matrix
    
    # Attempt to get the inverse stored in the special object
    m <- x$getinverse()
    # If it exists, use the cached value
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    # Retrieve the data stored in the special object
    data <- x$get()
    # Calculate the inverse
    m <- solve(data, ...)
    # Store the inverse in the special object
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
# special.object <- makeCacheMatrix(small.matrix)
# special.object$set(matrix)
# special.object$get()
# microbenchmark(cacheSolve(special.object)) # 115 microseconds
# special.object$getinverse()






