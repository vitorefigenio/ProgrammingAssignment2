# The makeCacheMatrix function creates a cache that stores the inverse matrix "x"

makeCacheMatrix <- function(x = matrix()) {
  data <- NULL
  set <- function(y) {
    x <<- y
    data <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) data <<- solve
  getinverse <- function() data
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# The cacheSolve function calculates the inverse of the matrix returned 
# by the function makCacheMatrix. If the reverse has been calculated and
# the matrix has not been modified, then the cacheSolve function retrieves
# the reverse cache.

cacheSolve <- function(x, ...) {
  data <- x$getinverse()
  if(!is.null(m)) {
    message("loading data")
    return(data)
  }
  y <- x$get()
  data <- solve(y)
  x$setinverse(data)
  data
}
