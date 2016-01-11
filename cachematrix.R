## This file contains pair of functions, which help
## caching the inverse of a given matrix

## makeCacheMatrix function takes as argument a matrix
## and returns a "matrix", which is really a list containing 4 functions,
## which set and get the value of the matrix, and set and get the value of
## the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <-  function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x 
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
            setinv = setinv,
            getinv = getinv)
}


## cacheSolve function checks to see if the inverse matrix has already been 
## calculated and if not, calculates the inverse of the special "matrix" 
## created with the above function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
