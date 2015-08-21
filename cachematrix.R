## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #print ("this is x!")
  invValue <- NULL
  set <- function(y) {
    x <<- y
    invValue <<- NULL
  }
  get <- function() x
  setInv <- function(solve) invValue <<- solve
  getInv <- function() invValue
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invValue <- x$getInv()
  if (!is.null(invValue)) {
    return(invValue)
  }
  data <- x$get()
  invValue <- solve(x)
  x$setInv(invValue)
  invValue
}
