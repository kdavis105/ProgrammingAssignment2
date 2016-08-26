# Functions create a special matrix object that can cache its inverse.


# makeCacheMatrix creates a list containing functions that 1. set the value 
# of the matrix, 2. get the value of the matrix, 3. set the inverse of the 
# matrix and 4. get the inverse of the martix.  This list is used as 
# the input to the cacheSolve() function.

makeCacheMatrix <- function(x = matrix()) {
  
  #  Initialize cache to NULL.
  cache <- NULL
  
  #  Create the matrix.
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  #  Get the value of the matrix.
  get <- function() x     
  
  #  Store inverted matrix in cache.
  setinv <- function(inverse) cache  <<- inverse
  
  #  Get the inverted matrix from cache.
  getinv <- function() cache
  
  #  Return list of functions.
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  
  #  CacheSolve() returns the inverse of the matrix. If the inverse of the 
  #  matrix has already been computed, it will not be re-computed 
  #  and will be retrieved from cache.  If the inverse of the matrix has 
  #  not been computed, it will be computed and the value cached with 
  #  setinv()
  
  #  Attempt to get matrix from cache.
  cache <- x$getinv()
  
  #  Return the matrix if the inverse has already been calculated and 
  #  is in cache.
  if (!is.null(cache)){
    
    message("getting cached data")
    return(cache)
  }
  
  # Get the matrix and calculate the inverse. 
  matrix = x$get()
  cache <- solve(matrix, ...)
  
  # Set the value of the inverted matrix in cache and return the matrix.
  x$setinv(cache)
  return(cache)
}


