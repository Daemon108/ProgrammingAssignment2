## Put comments here that give an overall description of what your
## functions do

## function makeCacheMatrix:
## - creates a special "matrix" object that can cache its inverse
## - creates 4 functions for manipulating a matrix and its inverse:
##      - set() / get() - to set / get a matrix
##      - setInverse() / getInverse() - to set / get an inversed matrix

## function cacheSolve calculates the inverse of the special 
## "matrix" created with the above function. However, it first checks
## to see if the inverse has already been calculated. If so, it gets
## the inverse from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the matrix and puts the result to the cache
## via the setInverse function.

## for our assignment we assume that the matrix supplied is always invertible
## Otherwise I would recommend to change the calculations of inverse to
## something like
## inv <- try(solve(data, ...),TRUE)
# if(!is.matrix(inv)) {
#         warning("problem inverting matrix:",inv)
#         inv<-NULL
#         return(inv)
# }

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## Return a list of 4 functions for manipulating the matrix
        ## and its inverse
        
        cachedInverse <- NULL
        set <- function(y) {
                x <<- y
                cachedInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) cachedInverse <<- inv
        getInverse <- function() cachedInverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Return a cached inverse if exists
        
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
