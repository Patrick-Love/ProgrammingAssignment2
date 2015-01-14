## These functions allow you to create a cache for the inverse of
## a matrix. If that same matrix is supplied again it will 
## return the cached inverse rather than doing the caclulation.

## The matrix is assumed to be invertible
## Tested on several square, invertible matrices (1/14/15).

## makeCacheMatric creates a matrix object that has methods to 
##cache the inverse of a matrix instance supplied by method 
## m$set (m).

makeCacheMatrix <- function(m = matrix()) {
  mflag <- NULL
  set <- function (y) {
    m <<- y
    mflag <<- NULL    ## signals that matrix has changed.
  }
  get <- function () m
  setinv <- function (m) {
    mflag <<- 1     ## signals --if set is not run again don't solve
    minv <<- m
  }
  getinv <- function () minv
  getflag <- function () mflag
  list (set = set, get = get,
        setinv = setinv, getinv = getinv, getflag = getflag)
}


## Cachesolve takes a matrix object and checks to see if 
## m$set ran thus making a new matrix instance.
## if m$set has not been run its uses the cached inverse of
## the matrix.

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
  mflag <- m$getflag()
  print (mflag)
  if (!is.null(mflag)) {    ##set not run again-same matrix use cache
    message ("getting cached data")
    minv <- m$getinv ()
    return (minv)
  }
  dmatrix <- m$get()        ## else - solve and set inverse
  minv <- solve (dmatrix, ...) ## assumes invertible
  m$setinv (minv)
  minv
}

