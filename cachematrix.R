## makeCacheMatrix caches a matrix in a new environment
## cacheSolve calculates the inverse and caches it also in same environment
## functions get, getinverse can be used to retrieve the matrices, giving
##   the environment as variable

## makeCacheMatrix takes a matrix given as input and caches it in new 
##   enviroment which is also returned by function

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse  <- function(solve) inverse <<- solve
        getinverse  <- function() inverse 
        list(set = set, get = get,
             setinverse  = setinverse,
             getinverse  = getinverse)
}

## cacheSolve needs to be given the enviroment as input
##   where the original matrix is cached. It then solves
##   for the inverse and also chaches it and returns it.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse  <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}