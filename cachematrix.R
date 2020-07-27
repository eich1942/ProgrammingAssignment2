## Creates a new matrix in cache
## Then returns the inverse

## It creates a "special matrix" that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  set_inv <- function(solve){ 
    s <<- solve}
  get_inv <- function() s
  list(set =set, get = get, set_inv= set_inv, get_inv=get_inv)
  
  
}

## Computes the inverse of the "special matrix"

cacheSolve <- function(x, ...) {
  s <- x$get_inv()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$set_inv(s)
  s
}


## Return a matrix that is the inverse of 'x'

