## R script containing two functions for caching the inverse of a matrix.
## These functions may be useful for optimizing loops when constantly
## computing the inverse of a matrix can be compuationally costly.
## The script accomplishes its task via two functions: makeCacheMatrix
## and cacheSolve. The script assumes the matrix it is given is
## invertible. Modified by motivatedtapir.

## makeCacheMatrix create a special "matrix" object (a list containing a function) 
## that:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse
## 4. gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated, and the
## matrix has not changed, then cacheSolve will retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}



