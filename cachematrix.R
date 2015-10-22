## This file contains the two functions makeCacheMatrix and cacheSolve
## These functions can be used to store a matrix, cache its inverse and get it back

## This function creates an object that is able to store a matrix together with
## its inverse
## The inverse is only available if it has been cached by the second function cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  # new variable for keeping the inversed matrix, initialized with NULL
  inversedmatrix <- NULL
  set <- function(y) {
    x <<- y
    inversedmatrix <<- NULL
  }
  get <- function() x
  # functions for setting and getting the inverse of a given matrix
  setinverse <- function(solve) inversedmatrix <<- solve
  getinverse <- function() inversedmatrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The function cacheSolve gets a matrix object and computes the inverse of the matrix.
## The inverse of the given matrix is stored in the makeCacheMatrix object and can
## be accessed via this object and its getinverse function

cacheSolve <- function(x, ...) {
  # get the inverse of the matrix to see if it has already been computed and cached
  inversedmatrix <- x$getinverse()
  # if it has already been cached, print a message and return the cached data
  if(!is.null(inversedmatrix)) {
    message("getting cached matrix data")
    return(inversedmatrix)
  }
  # if the inverse of the matrix has not been computed and cached before, do it now
  # first: get data out of the matrix object
  data <- x$get()
  # inverse of the matrix is computed by using the solve()-function
  inversedmatrix <- solve(data)
  # inverse is set with the function setinverse of the makeCacheMatrix object
  x$setinverse(inversedmatrix)
  inversedmatrix
}
