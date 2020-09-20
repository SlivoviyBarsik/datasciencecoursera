## The two functions below are supposed to enable the user to retrieve
## the inverse of matrix from cache if it has already been calculated prior
 

## makeCacheMatrix function creates a "matrix" object which itself is really 
## a list of functions which set/get the value of matrix/inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## cacheSolve computes the inverse of "matrix" from the above function and,
## if it has already been computed, retrieves it from cache

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
