# It creates a special matrix object that can cache its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  return(list(set = set,
              get = get,
              setInverse = setInverse,
              getInverse = getInverse))
}

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  return(list(set = set,
              get = get,
              setInverse = setInverse,
              getInverse = getInverse))
}

# This returns a matrix that is the inverse of x

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  return(inv)
}




# Testing the created function

source("./Scripts/Programming Assignment 2 - Lexical Scoping.R")

mat <- makeCacheMatrix(matrix(c(23,34,12,
                                      56,78,32,
                                      0,2,40), 
                              nrow = 3, ncol = 3, byrow=T))

mat$get()   # to see the created matrix
mat$getInverse()    # to see the inverse matrix if exists
cacheSolve(mat)   # inverses the matrix
mat$getInverse()    # now the inverse matrix will be shown
mat$set(matrix(c(20, 12, 41,94), 2, 2))   # set a new matrix
mat$get()   # see the new matrix
mat$getInverse()
cacheSolve(mat)
mat$getInverse()

