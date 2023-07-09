makeCacheMatrix <- function(x = matrix()) {
  ## Init invrs variable
  invrs <- NULL
  ## This function is used to set the value of the matrix.
  set <- function(y){
    ## Save matrix in chache
    x <<- y
    ## Save inversed matrix in chache
    invrs <<- NULL
  }
  ## Get matrix
  get <- function () x
  ## Set inverse matrix
  setInverse <- function(inverse) invrs <<- inverse
  ## Get inverse matrix
  getInverse <- function() invrs
  ## Return list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  ## Get inversed matrix
  invrs <- x$getInverse()
  ## If not null return inversed matrix
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  ## If null get matrix
  data <- x$get()
  ## Inverse matrix
  invrs <- solve(data, ...)
  ## Set inversed matrix to chache
  x$setInverse(invrs)
  ## Return a matrix that is the inverse of 'x'
  invrs
  
}
