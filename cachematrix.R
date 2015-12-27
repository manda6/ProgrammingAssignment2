## The following two functions are used to cache the inverse of a matrix, which can be a costly computation.

## The makCacheMatrix function creates a special "matrix" object that can cache its inverse by using 4 of the following functions:
## First, set matrix.
## Second, get matrix.
## Thrid, set value of inverse matrix.
## Fourth, get value of inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  ## assign m to be used as the special "matrix" variable
  m <- NULL
  
  ## set the matrix value
  set <- function(y) {
    
    ## Note: "<<-" operator used to assign a value to an object in an environment different from the current one
    x <<- y
    m <<- NULL
  }
  
  ## get the matrix value
  get <- function() x
  
  ## set inverse value of the matrix
  settinginverse <- function(inverse) m <<- inverse
  
  ## get inverse value of the matrix
  gettheinverse <- function() m
  
  ## list created to be used with the cacheSolve function
  list(set = set, get = get,
       settinginverse = settinginverse,
       gettheinverse = gettheinverse)
  
  
}


## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the result from the cache.
## If no inverse has been calculated, the calculation will be computed, then cached.
## This will save time for large computations since we can look up the cached inverse, for instance, instead of recalculating it each time.  

cacheSolve <- function(x, ...) {
  ## Assign m variable to use as the inverse of the matrix input from the makeCacheMatrix function.
  m <- x$gettheinverse()
  
  ## If the calcuation has already been done (the inverse of x returns a value), retrieve cached data.
  ## Skip computation.
  if(!is.null(m)) {
    message("Retrieving the cached data...")
    return(m)
  }
  
  ## However, if m is null above, then the inverse has not yet been calculated.
  ## Compute the inverse using the solve function
  data <- x$get()
  m <- solve(data, ...)
  
  ## set the value of the inverse to now be cached - use settinginverse function above in makeCacheMatrix()
  x$settinginverse(m)
  
  ## Return the matrix (variable m) that is the inverse of 'x'
  return(m)
}
