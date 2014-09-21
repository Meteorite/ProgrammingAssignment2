## Programming assigment 2 on R Programming course on coursera - test
## Alexey Meteorite

## Tests makeCacheMatrix and cacheSolve with some very simple test cases
## Stops on error

testCacheMatrix <- function() {
  isInverse <- function(x, y) {
    isTRUE(ncol(x) == ncol(y)) &
      isTRUE(nrow(x) == nrow(y)) &
      all.equal(x %*% y, diag(ncol(x)))
  }
  
  m <- makeCacheMatrix(rbind(c(2,1,4),c(1,0,1),c(5,1,6)))
  stopifnot(is.null(m$getinverse()))
  inv <- cacheSolve(m)
  stopifnot(isInverse(inv, m$get()))
  stopifnot(all.equal(inv, m$getinverse()))
  inv <- cacheSolve(m)
  stopifnot(all.equal(inv, m$getinverse()))
  m$set(cbind(c(1,2),c(3,4)))
  stopifnot(is.null(m$getinverse()))

  testInverse <- function(x) {
    xinv <- cacheSolve(makeCacheMatrix(x))
    ##print(xinv)
    isInverse(x, xinv)
  }
  
  stopifnot(
    testInverse(rbind(c(1,0),c(0,-1))),
    testInverse(rbind(c(1,0),c(0,1))),
    testInverse(rbind(c(2,3,4),c(1,2,-3),c(4,5,6))))
}
