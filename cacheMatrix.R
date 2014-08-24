## These functions ar used to return the inverse o f matrix x by first setting a list
## which contains a function with relevant elements and then checking the cache to see 
## if the inverse hs already been calculated, if it has,  it returns the inverse with no calculation
## if it hasn't, then does the calculation and return the inverse.

## This function creates a list that contains a function to set and get both the matrix and 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Returns the inverse of a matrix created by the makeCacheMatrix function 
## which is used as input.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv<- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
