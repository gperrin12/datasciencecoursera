## first function creates a special "matrix", which is a list
## containing a function to set the value of the matrix,
## get the value of a matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
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


## This function computes the inverse of matrix x with 'solve'
## if the inverse m has already been solved, then the value of
## the inverse of m is returned. if not, then the matrix inverse
## is solved and that value is returned
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