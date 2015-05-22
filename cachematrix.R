## Caching the inverse of Functions
## Creating by Laura Perry May 22nd 2015


## This function creates a special "matrix" object that can
## cache its inverse
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cacheSolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv))
  {
    message("getting cached inverse")
    return(inv)
  }
  ## solve the inverse of the matrix after retrieving it
  m <- x$get()
  inv <- solve(m)
  
  ## cache the matrix inverse
  message("caching matrix")
  x$setInv(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}
