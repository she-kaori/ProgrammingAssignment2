## Coursera - Johns Hopkins University  - R Programming
## Programming Assignment 2: Lexical Scoping
## Caching the Inverse of a Matrix
## 
## (assume that the matrix supplied is always invertible)
##
##
#############################################################
##
##
## makeCacheMatrix creates a special "matrix" object that can cache 
## its inverse.
## 
## cacheSolve computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.



## creates an object that is a list with functions to 
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## this function returns a matrix that is the inverse of 'x'
## it first checks if the inverse has already been calculate
## if so, it gets the inverse from the cache
## otherwise, it calculates the inverse of x
## and sets the inverse in the cache
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
