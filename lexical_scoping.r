makeCacheMatrix <- function(x=matrix()) {
  inverse_x <- NULL
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  get <- funtion() x
  set_inverse <- function(inverse) inverse_x <<- inverse
  get_inverse <- function() inverse_x
  list(set = set, get= get, setinv = set_inverse, get = get_inverse)
}

cacheSolve <- function(x, ...) {
  inverse_x <- x$get_inverse()
  if(!is.null(inverse_x)) {
    message("getting cached data")
    return (inverse_x)
  }
  data <- x$get()
  inverse_x <- solve(data, ...)
  x$set_inverse(inverse_x)
  inverse_x
}