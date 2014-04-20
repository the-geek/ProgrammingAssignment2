## Functions to create a potential "performance-enhanced" matrix object that
## caches the inverse to avoid re-calculating it for the same matrix


## makeCacheMatrix returns a list of setters and getters functions that operates
## on the input matrix object to support inverse caching

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


## cacheSolve returns the cached inverse value if available otherwise it calculates
## the inverse and cache it

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv  
}
