## Put comments here that give an overall description of what your
## functions do
## Submitted by: Heng Rui Jie
## Coursera Course: R Programming
## Project: Programming Assistant 2

## Write a short comment describing this function

## Description of makeCacheMatrix
## The makeCacheMatrix creates a special matrix object that 
## consists of get, set, getInverse and setInverse function
## get and set are the respective getter and setter of the special matrix object
## getInverse retrieve the cache Inversed matrix while 
## setInverse updates the Matrix of its inverse value

makeCacheMatrix <- function(x = matrix()) {
  
  # initialize the inverse to null
  i <- NULL
  
  # if the set function is called, assume a new matrix data
  # is added. Initialize the inverse to null to notify that
  # the inverse needs to be re-computed
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  
  # return the special matrix list object
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function

# Description of cacheSolve
# Basically gets the input value x
# Determines if the inverse has already been computed
# If yes, then get the cached data
# If no, compute the inverse and store it in x
# The return value is the inverse of the input x

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matrixData <- x$get()
  i <- solve(matrixData, ...)
  x$setInverse(i)
  i
  
  }
