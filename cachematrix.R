## These functions create a matrix whose inverse can be cached.

## Wrap the given matrix inside an object that allows keeping its
## inverse as a cache.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(newInverse) inverse <<- newInverse
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Solve the inverse of the given matrix, taking advantage of the
## caching ability, returning the cached value if it has been solved
## already. The parameter x must be constructred from a matrix using
## the makeCacheMatrix function, and must be an invertible matrix.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("Getting cached data")
    return(inverse)
  }
  inverse <- solve(x$get())
  x$setInverse(inverse)
  inverse
}
