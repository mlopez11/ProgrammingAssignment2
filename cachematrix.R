# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly 
# Your assignment is to write a pair of functions that cache the inverse of a matrix.

# The first function, makeVector creates a special "vector", which is really a list containing a function to
# set the value of the matriz
# get the value of the matrix
# set the value of inverse matrix
# get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) minv <<- inverse
    getinverse <- function() minv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



# Return a matrix that is the inverse of 'x'
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
    minv <- x$getinverse()
    if(!is.null(minv)) {
        message("getting cached data.")
        return(minv)
    }
    data <- x$get()
    minv <- solve(data, ...)
    x$setinverse(minv)
    minv
}

## EXEMPLE OF SIMPLE EXECUTION
# mymat <- matrix (1:4, nrow=2, ncol=2) 
# mymat
# my = makeCacheMatrix(mymat)
## FIRST TIME WITHOUT CACHE
# cacheSolve(my)
# my$get()
# my$getinverse()
## SECOND TIME WITH CACHE
# cacheSolve(my)
# VERIFY
# mymat %*% my$getinverse()

## EXEMPLE OF LARGE EXAMPLE WITH TIME EXECUTION
# mymat = matrix(rnorm(1000000), nrow=1000, ncol=1000)
# my = makeCacheMatrix(mymat)
# system.time(cacheSolve(my))
# system.time(cacheSolve(my))
# det(my$get() %*% my$getinverse())

