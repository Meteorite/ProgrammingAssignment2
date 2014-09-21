## Programming assigment 2 on R Programming course on coursera
## Alexey Meteorite

## Wraps square inversible matrix into object,
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) m <<- mean
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns inverse of matrix-object.
## Takes it from cahce, if already calculated,
## else calculates and caches it

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(is.null(inv)) {
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
  } else {
    message("getting cached data")
  }
  inv
}
