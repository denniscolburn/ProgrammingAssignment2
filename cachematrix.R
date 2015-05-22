## The functions in this file enable to user to create matrices
## and the inverse of matrices.  Inverses of matrices can be
## resource intensive to generate, so the functions also enable
## users to cache and set the value of a matrix's inverse instead
## of recalculating it each time.


## Function: makeCacheMatrix
##
## Description: Creates an object that enables a user to set the 
## value of a matrix and its inverse and also return their values.
##
## Input
##   x (matrix) [defaults to empty matrix]
##
## Output 
##   List of functions (described below)
##     set - sets the value of the matrix and initializes the inverse to NULL
##     get - returns the value of the matrix
##     setInverse - sets the value of the matrix's inverse
##     getInverse - gets the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  getInverse <- function() {
    i
  }
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Function: cacheSolve
##
## Description: Returns the matrix that is the inverse of 'x'.  If
## the inverse value is cached, then it returns the cached value.  
## Otherwise, it calculates the inverse using solve().
##
## Input
##   x (matrix) 
##
## Output 
##   i (inverse of matrix 'x')

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  
  if ( !is.null(i) ) {
    message("Getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i

}