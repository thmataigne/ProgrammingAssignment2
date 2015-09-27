## These functions compute the inverse of a matrix, and cache the result
## so it can be quickly obtained later on without recomputing it

## This function creates a matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  ## set will set object x to matrix value given in y
  ## cached inverse is set to null
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  ## get returns the (usual) matrix
  get <- function () x
  ## setSolve stores inverse in s
  setSolve <- function(inv) s<<-inv
  ## getSolve returns caches matrix or null if not yet computed
  getSolve <- function() s
  ## finally return a list of these functions
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
  ## when calling the getSolve of x, it gives either the
  ## cached inverse or NULL if it's not yet computed
  if ( !is.null(s) ) {
    ## when cached, return the available inverse
    message("getting cached data")
    return(s)
  }
  ## otherwise compute it and put it in cache
  data <- x$get()
  ## solve computes the inverse
  s <- solve(data,...)
  ## the setSolve of x stores result in cache
  x$setSolve(s)
  ## and we return the just computed inverse
  s  
}
