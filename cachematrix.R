## makecacheMatrix to empty the cache.
##
## Rama Camara 16/04/2016
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## The solve function requires a square matrix
##
## Example using a square matrix 2x2
## sqm <- matrix(1:4,2,2)
## sqm
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## 
## cachedm <- makeCacheMatrix()
## cachedm$set(sqm)


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## set non-inverted matrix and clear cache
  
  set <- function(y) {
    ## set x in the to the passed value of y
    x <<- y
    ## set m (matrix) to null to clear the cache for inverted matrix
    m <<- NULL
  }
  
  ## get original matrix
  get <- function() x
  ## set inverted() matrix
  setinverted <- function(solve) m <<- solve
  ## get inverted matrix
  getinverted <- function() m
  
  ## Create a list of the member functions describing the functions available
  list(set = set, get = get,
       setinverted = setinverted,
       getinverted = getinverted)
}

## this is an example of the cachesolve behaviour
## cachedm$set(sqm)
## cacheSolve(cachedm)
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## cacheSolve(cachedqm)
## getting cached data
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

cacheSolve <- function(x, ...) {
  ## get inverted?
  m <- x$getinverted()
  
  ## did we get anything
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } else {
    ## get non-inverted and populate inverted.
    data <- x$get()
    m <- solve(data, ...)
    x$setinverted(m)
    ## return the inverted matrix
    return(m)
  }
}
