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
# [,1] [,2] [,3]
# [1,]   23   34   12
# [2,]   56   78   32
# [3,]    0    2   40
mat$getInverse()    # to see the inverse matrix if exists
# NULL
cacheSolve(mat)   # inverses the matrix
# [,1]        [,2]        [,3]
# [1,] -0.67491166  0.29505300 -0.03356890
# [2,]  0.49469965 -0.20318021  0.01413428
# [3,] -0.02473498  0.01015901  0.02429329
mat$getInverse()    # now the inverse matrix will be shown
# [,1]        [,2]        [,3]
# [1,] -0.67491166  0.29505300 -0.03356890
# [2,]  0.49469965 -0.20318021  0.01413428
# [3,] -0.02473498  0.01015901  0.02429329
mat$set(matrix(c(20, 12, 41,94), 2, 2))   # set a new matrix
mat$get()   # see the new matrix
# [,1] [,2]
# [1,]   20   41
# [2,]   12   94
mat$getInverse()
# NULL
cacheSolve(mat)
# [,1]        [,2]
# [1,]  0.067723343 -0.02953890
# [2,] -0.008645533  0.01440922
mat$getInverse()
# [,1]        [,2]
# [1,]  0.067723343 -0.02953890
# [2,] -0.008645533  0.01440922

