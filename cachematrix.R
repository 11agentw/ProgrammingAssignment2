## These two functions allow for the inverse of a matrix to be cached in order 
## to avoid repeating the time-consuming calculation.

## 'makeCacheMatrix' creates a list containing four functions:
##    set - set the value of a matrix
##    get - return the matrix value
##    setinverse - set the inverse of the stored matrix
##    getinverse - return the stored inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- cacheSolve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## 'cacheSolve' is used in conjunction with 'makeCacheMatrix$setinverse' in
## order to calculate a matrix that is the inverse of 'x'. Before trying to
## solve for the matrix, the function checks to see if the solution is already
## cached and need not be calculated again.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  ## Error checks for a non-square matrix and the determinant of the matrix
  ## being equal to zero.
  
  if(nrow(data) != ncol(data)) {
    stop("The inverse of a non-square matrix cannot be calculated.")
  }
  if(det(data) == 0) {
    stop("A matrix with a determinant of zero has no inverse.")
  }
  
  ## Solve for the inverse of the matrix and set cache
  
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
