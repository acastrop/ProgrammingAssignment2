# makeCacheMatrix is our function
# Store a matrix and get a cached value of inverse matrix. 
# Contains this functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cacheInverse   get the cached value (inverse matrix)
# * getInverse     get the cached value (inverse matrix)
#
makeCacheMatrix <- function(x = numeric()) {
  
  # hold cached value or NULL 
  cache <- NULL
  
  # store a matrix
  setMatrix <- function(newValue) {
    x <<- newValue
    cache <<- NULL
  }
  
  # returns the stored matrix
  getMatrix <- function() {
    x
  }
  
  # cache argument 
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  # get the cached value
  getInverse <- function() {
    cache
  }
  
  # return a list
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# Function calculates the inverse of a matrix created with makeCacheMatrix
cacheSolve <- function(y, ...) {
  # get cached value
  inv <- y$getInverse()
  # if cached value exists return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # otherwise get matrix, calc inverse & store in cache
  data <- y$getMatrix()
  inv <- solve(data)
  y$cacheInverse(inv)
  
  # return m inverse
  inv
}