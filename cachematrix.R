## Put comments here that give an overall description of what your
## functions do

## First halve creates empty variables to fill later

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinversion <- function(inverse) inv <<- inverse
  getinversion <- function() inv
  list (set = set, get = get, setinversion = setinversion, getinversion = getinversion)
    
}


## This function looks whether a calculation has already been made in the cache, if so, it retrieves it, if not, it calculates it.

cacheSolve <- function(x, ...) {
  inv <- x$getinversion()
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$getinversion(inv)
  
  inv
}
