##
## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly. 
##

## Following pair of functions will cache the inverse of a matrix.

## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.
## Input of this function should be a matrix, X

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- matrix()
  setMatrix <- function(y) {
    x <<- y
    x_inv <<- matrix()
  }
  getMatrix <- function() {
    x
  }
  setInv <- function(xi = matrix()) {
    x_inv <<- xi
  }
  getInv <- function(){
    x_inv
  }
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve: 
## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix function above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.

## The function computes the inverse of a matrix using Solve() function.
## The input to the function should be a square invertible matrix, X.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xInv <- matrix()
  xInv <- x$getInv()
  xdata <- x$getMatrix()
  if(is.na(xInv)) {
    if (nrow(xdata)==ncol(xdata)) {
      ##xdata <- x$getMatrix()
      xInv <- solve(xdata, ...)
      x$setInv(xInv)
      
    } 
    else
    {
      message("Matrix inversion is not possible since the matrix is not a square matrix.")
      return(NULL)
    }
  }
  else
  {
    message("Getting matrix inverse from cached data...")
  }
  return(xInv)
}
