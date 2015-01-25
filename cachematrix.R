## Matrix inversion can be a costly computation that can take a lot of time;
## The following 2 functions are written to save time by caching the inverse
## of a matrix rather than compute it each time 

## makeCacheMatrix is a function that creates a special 
## "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse)
  
}


## cacheSolve is a function that computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cachesolve
## will retrieve the inverse from the cache in place of recalculating the
## entire inverse.

cacheSolve <- function(x, ...) {
 
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("checking for cached data in place of 
              recalculating the inverse")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
  
}

