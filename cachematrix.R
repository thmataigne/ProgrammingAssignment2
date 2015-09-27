## These functions compute the inverse of a matrix, and cache the result
## so it can be quickly obtained later on without recomputing it

## This function creates a matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function () x
  setSolve <- function(inv) s<<-inv
  getSolve <- function() s
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## This function computes the inverse of a matrix x constructed
## with makeCacheMatrix. It uses the cached value or computes a new one
## if it is not in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getSolve()
  if ( !is.null(s) ) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data,...)
  x$setSolve(data,...)
  s  
}
