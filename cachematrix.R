## Put comments here that give an overall description of what your
## functions do

## TJH 12-23-15 This function creates a capability that lets
## user add a cached copy of a matrix for use with other functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinvMatrix <- function(inverse) inv <<- inverse
  getinvMatrix <- function() inv
  list(set = set, get = get,
       setinvMatrix = setinvMatrix,
       setinvMatrix = setinvMatrix)
}

## TJH 12-23-15 This function will reference a the matrix 
## and functions of makeCacheMatrix in order to inverse and return 
## inverse of matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinvMatrix()
  if(!is.null(inv)) {
    message("getting cached Matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinvMatrix(inv)
  inv
}
