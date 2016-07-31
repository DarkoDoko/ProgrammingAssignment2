## Matrix inversion is potentially time consuming process.
## In order to speed up the process we eill try caching the matrix inverse.

## This function creates special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    set <- function(y){
        x <<- y
        inverse <<- NULL
    }

    get <- function() x

    setInverse <- function(newInv) inverse <<- newInv
    getInverse <- function() inverse

    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()

    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }

    data <- x$get()
    inv <- solve(data)
    
    x$setInverse(inv)
    inv
}
## Sample run:
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 
