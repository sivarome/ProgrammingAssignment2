#
## Matrix inversion is usually a costly computation. To avoid unnecessary
## computation it is good to cache the inverse of a matrix.
##

## Following pair of functions will cache the inverse of a matrix.

## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.
## Input of this function should be a matrix, x
##  setMatrix  -  To set new matrix value
##  setInv     -  To cache the inverse of the matrix
##  getMatrix  -  To get current matrix contents
##  getInv     -  To get the cached inverse of the matrix
##
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

  xInv <- x$getInv()
  xdata <- x$getMatrix()
  
  ## check if the input matrix objext has a square invertible matrix
  if (nrow(xdata)!=ncol(xdata)) {
    message ("Please input a square matrix...")
    return(NULL)
  }
  if (det(xdata) == 0) {
    message ("Matrix inverse not possible for singular matrix...")
    return(NULL)
  }
  
  ## Find the matrix inverse
  if(is.na(xInv[1,1])) {
    xInv <- solve(xdata, ...)
    x$setInv(xInv)
  }
  else
  {
    message("Getting matrix inverse from cached data...")
  }
  return(xInv)
}
