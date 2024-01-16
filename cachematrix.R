## This pair of functions creates a special "matrix" object that can cache its inverse. 
## This is useful for speeding up matrix calculations by avoiding redundant 
## computation of the inverse.

## 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # This function sets the value of the matrix and resets the inverse to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # This function returns the value of the matrix
  get <- function() x
  # This function sets the value of the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  # This function returns the value of the inverse of the matrix
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## 'cacheSolve' computes the inverse of the special "matrix" returned by 'makeCacheMatrix'.
## If the inverse has already been calculated and the matrix has not changed, 
## then it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  # Checks if the inverse is already calculated
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # If the inverse is not in cache, calculates the inverse, caches it, and returns it
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  return(inv)  # Return a matrix that is the inverse of 'x'
}
