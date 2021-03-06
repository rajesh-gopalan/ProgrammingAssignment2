## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
##
## Author: Rajesh Gopalan
## E-mail: rajeshgopalan@gmail.com
## Course: R-Programming (coursera.org) - Week 3 assignment
##
## makeCacheMatrix: This function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  invertMatrix <- function(invertedMatrix) inverseMatrix <<- invertedMatrix
  matrixInverted <- function() inverseMatrix
  list(set = set, get = get,
       invertMatrix = invertMatrix,
       matrixInverted = matrixInverted)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
##
## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.
## 
## Computing the inverse of a square matrix can be done with the solve function in
## R. For example, if X is a square invertible matrix, then solve(X) returns its
## inverse.
## 
## For this assignment, assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$matrixInverted()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  dataMatrix <- x$get()
  inverseMatrix <- solve(dataMatrix, ...)
  x$invertMatrix(inverseMatrix)
  inverseMatrix
}
