## Functions to cache a matrix and its inverse.

## makeCacheMatrix creates setter and getter functions for the matrix and its inverse.
## Return a list containing the four functions.

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinv <- function(inv) invx <<- inv
  getinv <- function() invx
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSove determines if the matrix's inverse has already been cached. If so, return the cached inverse. 
## Else, calculate and return the inverse. Assumes that the matrix is square and invertible.

cacheSolve <- function(x, ...) {

  invx <- x$getinv()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setinv(invx)
  invx
}
