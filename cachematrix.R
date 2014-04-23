## This function creates a special "matrix" object that can cache its inverse.
##
## Internal methods can be called as follows:
##
## - get(): gets the current matrix
## - set(newMatrix): sets the current matrix
## - getInverse(): gets the current inverted matrix
## - setInverse(newMatrix): sets the cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  # need to intialize this outside this environment
  cacheMatrix <<- NULL
  
  # returns the current matrix
  get <- function() x
  
  # sets the matrix
  set <- function(newMatrix) {
    x <<- newMatrix # reset the matrix
    cacheMatrix <<- NULL # blow away the cached one
  }
  
  # returns the cached inverted matrix
  getInverse <- function() cacheMatrix
  
  # sets the cached inverted matrix
  setInverse <- function(newMatrix) {
    cacheMatrix <<- newMatrix
  }
  
  # expose the function signatures
  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  # get the cached inverted matrix
  invMatrix <- x$getInverse()
  
  # do we have a cached matrix?
  if (!is.null(invMatrix)) {
    #print('Found cached data!')
    
    return (invMatrix)
  } else { # will need to get the matrix and invert it
    invMatrix <- solve(x$get(), ...)
    
    x$setInverse(invMatrix)
    
    return(invMatrix)
  }
}



                                                                                                                               x2 %*% x
