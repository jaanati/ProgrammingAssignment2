## The function makeCacheMatrix caches the inverse of a square matrix and creates a special matrix object 
## The function cacheSolve uses the special object and retruns the inverser matrix if it is cached or computes it

## the funciton creates a 'special' matrix object which caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  rc <- dim(x)
  if(rc[1]!=rc[2])
    stop("input matrix should be square")
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinverse <- function() minv <<- solve(x)
  getinverse <- function() minv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




## the function below calcualtes the inverse of the 'special' matrix that is returned by the above function. 
## It checks if the inverse has been calculated already, if so, retrives it from the cache, else computes it. 


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  minv <- x$getinverse()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  rc <- dim(data)
  if(rc[1]!=rc[2])
    stop("input matrix should be square")
  
  minv <- solve(data, ...)
  x$setinverse()
  return(minv)
 
}
