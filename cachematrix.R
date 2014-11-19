## The makeCacheMatrix and cacheSolve functions create a special matrix object
## that can cache its inverse and allow the inverse to be retreived or, if it
## hasn't been calculated already, to be calculated and then returned.


## The makeCacheMatrix function takes a square invertible matrix as its argument
## and returns a list of functions. These functions get and set a matrix object 
## and allow its inverse to be cached.

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


## The cacheSolve function takes a makeCacheMatrix object as its primary argument
## and returns the inverse of the matrix contained in the object. If the object does
## not already contain the inverse or the matrix has been modified since the inverse
## was computed, the inverse is recalculated and returned.

cacheSolve <- function(x, ...) {
  mat <- x$get()
  inv <- x$getinverse()
  if(!is.null(inv)) { ## test whether cached inverse present
    if(all.equal(mat %*% inv, diag(nrow(mat)))){ ## test whether cached inverse still valid
      message("getting cached data")
      return(inv)
    }
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

