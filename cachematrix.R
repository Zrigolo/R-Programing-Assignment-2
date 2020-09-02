## Caching the Inverse of a Matrix:
## Matrix inversion is a costly computation and it can be beneficial
## to cache the inverse of a matrix instead of computing it repeatedly.
## The following functions are used to create a special object that
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that may cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y){
    x <<- y
    z <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) z <<- inverse
  getInverse <- function() z 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" created by
## previous function. If the inverse was calculated already(and the
## matrix has not changed), then it will retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  z <- x$getInverse()
  if(!is.null(z)){
    message("getting cached data")
    return(z)
  }
  mat <- x$get()
  z <- solve(mat,...)
  x$setInverse(z)
  z
}