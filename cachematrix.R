## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(y = matrix()) { ## the function is a list
  inv <- NULL                              
  set <- function(x) {  ## set the value of the matrix
    y <<- x                             
    inv <<- NULL                        
  }
  get <- function() y   ## get the value of the matrix
  setinverse <- function(inverse) inv <<- inverse  ## set the value of the inverse
  getinverse <- function() inv   ## get the value of inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)                                                                                
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache.
cacheSolve <- function(y, ...) {    
  inv <- y$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- y$get()
  inv <- solve(data, ...)
  y$setinverse(inv)
  inv
}