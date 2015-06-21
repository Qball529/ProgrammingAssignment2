## Calculate inverse of a matrix. If it had already been calculated, just get it from memory.


## get: retrieves a stored matrix
## set: set original matrix into memory
## setinv: store calculated inverse matrix into memory
## getinv: if an inverse previously had been calculated, get it from memory
makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setinv <- function(inv) a <<- inv
  getinv <- function() a
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## check if an inverse for a matrix had been calculated
## If there's one stored in memory, get it from memory.
## If not, calculate it and store in memory using x$setinv(a)

cacheSolve <- function(x, ...) {
  a <- x$getinv()
  if(!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setinv(a)
  a
}
