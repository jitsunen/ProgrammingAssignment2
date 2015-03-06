# R-Programming Assignment2                                     
# Author:        jitsunen
# Code created:  6th Mar 2015                                                   
#
# This script contains two functions to help compute and cache the inverse of a matrix.
# The function makeCacheMatrix returns a list of functions which provide caching ability to a 
# passed in matrix. The function cacheSolve accepts a matrix created through makeCacheMatrix function
# and returns its inverse either from a cache or computes the inverse if it was not found in the cache.

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
  # Computes the inverse of a passed in matrix, which was created through the makeCacheMatrix function, 
  # and returns it. If the inverse was already computed by this function on a previous invocation then it 
  # simply returns the computed inverse from the cache.
  #
  # Args:
  #  x: matrix whose inverse is to be made cacheable.
  #  ...: arguments passed to solve() to compute inverse of x.
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
