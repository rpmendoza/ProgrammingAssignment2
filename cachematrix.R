## cachematrix.R
## Author: R. Mendoza
## Date: 8/21/15
## The cachematrix.R file contains two functions that cacluclates an inverse
## of a matrix and caches the results. When the same matrix is passed to
## the function, it gets the cached results instead of re-caclulating
## the inverse of a matrix again.

## The makeCacheMatrix function is a list of functions that will
## be used when makeCacheMatrix is assigned to a matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## makeCacheMatrix$set resets the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## makeCacheMatrix$get gets the value of the matrix
  get <- function() x
  
  ## makeCacheMatrix$setInv stores the value of the matrix into variable m
  setInv <- function(solve) m <<- solve
  
  ##makeCacheMatrix$getInv returns the value of the matrix
  getInv <- function() m
  
  ## creates a list of the four functions
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## The cacheSolve function caclulates the inverse of a matrix.
## If the same matrix is passed, it simply gets the cached 
## it already calculated instead of re-calculating it again.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  
  ## checks if the inverse already exists
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## if the inverse does not exist, data is passed to the
  ## get() function, then gets solved. It is then stored in
  ## the setInv() function.
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
  
  ## to test the code, run the following:
  ## > source("cachematrix.R")
  ## > a <- matrix(1:4, 2, 2)
  ## > b <- makeCacheMatrix(a)
  ## > cacheSolve(b)
  ## 
  ## this calculates the inverse of matrix a
  ##
  ## > cacheSolve(b)
  ## since the matrix did not change, running cacheSolve(b) again
  ## will get the cached inverse instead of recalculating
}
