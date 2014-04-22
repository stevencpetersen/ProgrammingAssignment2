## These functions combine to calculate, cache, and return the inverse of a matrix. The first
## function creates a vector containing functions that set the value of the matrix, 
## get the value of the matrix, calculate and set the inverse of the matrix, and 
## get the inverse of the matrix.The second function tests for a cached inverse and 
## returns it if present, otherwise calculates the inverse and returns it.

## This funtion creates a vector of functions that support the calculation, cache, and recall
## of the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) m <<- solve
      getInverse <- function() m
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)    
}


## This function tests for a cached inverse of a matrix and if present returns it,
## otherwise it calculates the inverse, caches it, and returns it.

cacheSolve <- function(x, ...) {
      m <- x$getInverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setInverse(m)
      m    
}