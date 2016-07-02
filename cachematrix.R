## makeCacheMatrix returns a list of functions which compute the inverse of a square 
## matrix and cache the result; cacheSolve checks to see if the inverse of a square 
## matrix has been cached and returns the cached solutions or computes the inverse

## makeCacheMatrix returns a list of functions which compute the inverse of a square matrix
## and cache the result

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve checks to see is the inverse of a matrix has been cached and returns 
## that inverse; otherwise, it computes the inverse

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
