## Lexical scoping, takes a matrix object and returns the inverse of this
## caches the result in memory so as to not need to compute the inverse each time on the same matrix if unchanged
# please note: the full code below needs to be run for this to work and the matrix to test
# is initialised at the bottom called variable b.

a <- matrix(c(1,2,3,4), nrow=2, ncol=2) # create a matrix to put into the function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## temporarily stores the solution as a variable and returns this if it has already been calculated otherwise solves the
# inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    
}

b <- makeCacheMatrix(a) # initialise the test matrix
