# R-Programming Assignment2                                     
# Author:        jitsunen
# Code created:  6th Mar 2015                                                   
#
# This script contains two functions to help compute and cache the inverse of a matrix.
# the function makeCacheMatrix returns a list of functions which provide caching ability to a 
# passed in matrix. The function cacheSolve utilizes the matrix created through makeCacheMatrix
# to check the cache before computing its inverse.


makeCacheMatrix <- function(x = matrix()) {
  # Construct and return a list of functions to cache and return the inverse of a passed in matrix.
  #
  # Args:
  #  x: matrix whose inverse is to be made cacheable.
  #  
  # Returns: 
  #  A list containing the functions get, setinverse, getinverse. 
  #  get returns the passed in matrix x.
  #  setinverse stores the computed inverse of x in the makeCacheMatrix function's environment.
  #  getinverse returns the stored inverse of x from makeCacheMatrix function's environment.
  
  cachedinverse <<- NULL
  get <- function() x
  setinverse <- function(inverse) cachedinverse <<- inverse
  getinverse <- function() cachedinverse
  list(get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  # Computes the inverse of a passed in matrix, created through the makeCacheMatrix, and returns it. 
  # If the inverse has already been computed by this function then it simply returns the computed 
  # inverse from its cache.
  #
  # Args:
  #  x: matrix whose inverse is to be made cacheable.
  #  ...: arguments passed to solve to compute inverse of x.
  #  
  # Returns: 
  #  inverse of matrix x
  
  cachedinverse <- x$getinverse()
  if(is.null(cachedinverse))
  {
    message("calculating and caching inverse for matrix")
    matrixtoinvert = x$get()
    cachedinverse <- solve(matrixtoinvert, ...)
    x$setinverse(cachedinverse)
  }
  cachedinverse
}
