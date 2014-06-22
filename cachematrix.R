## Programming Assignment 2, Sunday 06/22/14
## First function - makeCacheMatrix - calculates the inverse of a
## square non-singular invertible matrix, and caches it
## Second function - cacheSolve - checks to see if inverse has been solved for,
## if not solve for it, and if yes get cached data

## First function calculates the inverse of a non-singular invertible square matrix
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Second function checks for whether inverse has been calculated
## If not, calculate, else display cached inverse
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
