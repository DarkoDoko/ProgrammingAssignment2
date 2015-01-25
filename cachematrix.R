## Matrix inversion is potentially time consuming process.
## In order to speed up the process we eill try caching the matrix inverse.

## This function creates special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    set <- function(y){
        x <<- y
        m <<- NULL
    }

    get <- function() x

    setInverse <- function(solve) m <<- solve
    getInverse <- function() m

    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }

    data <- x$get()
    m <- solve(data)

    x$setInverse(m)

    m
}
