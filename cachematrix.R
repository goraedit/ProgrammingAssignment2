## Caching the inverse of a Matrix
## The program contains two functions. The first (makeCacheMatrix) define a special Matrix 
## variable, the next (cacheSolve) compute it's inverse.
##
## The program's usage:
## > source("cachematrix.R")
## > aMatrix <- makeCacheMatrix(matrix(c(4,5,3,59,85,78,44,1,-5),3,3))
## > cacheSolve(aMatrix)
##             [,1]        [,2]         [,3]
## [1,] -0.090143369  0.66792115 -0.659677419
## [2,]  0.005017921 -0.02724014  0.038709677
## [3,]  0.024193548 -0.02419355  0.008064516
## > cacheSolve(aMatrix)
## getting cached data
##             [,1]        [,2]         [,3]
## [1,] -0.090143369  0.66792115 -0.659677419
## [2,]  0.005017921 -0.02724014  0.038709677
## [3,]  0.024193548 -0.02419355  0.008064516


## This function creates a special Matrix that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setinvm <- function(solve) invm <<- solve
  getinvm <- function() invm
  list(set = set, get = get,
       setinvm = setinvm,
       getinvm = getinvm)
}


## This function computes the inverse of the special square Matrix. If the inverse has already 
## been calculated it returns with the cached value

cacheSolve <- function(x, ...) {
  invm <- x$getinvm()
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data, ...)
  x$setinvm(invm)
  invm
}
