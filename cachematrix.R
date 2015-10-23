## Functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ### inverse initialized as NULL, inverse has not been cached
  m <- NULL
  
  ### set matrix and reset inverse
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ### return matrix
  get <- function() x
  
  ### cache value passed into function
  ### value cached should be inverse of matrix stored in set function
  setinverse <- function(solve) m <<- solve
  
  ### return value cached in setinverse function
  getinverse <- function() m
  
  ### return four functions defined above in a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function caches the inverse of the matrix
## The cacheSolve uses the functions stored by the function makeCacheMatrix

cacheSolve <- function(x, ...) {
  ### get cached inverse
  m <- x$getinverse()
  
  ### if inverse is cached, return it
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  
  ### if inverse of x is not cached, solve for inverse, cache the inverse
  ### and return the inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
