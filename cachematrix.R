##inspired by the example of cache the vector
##I transformed the code to solve this problem
##the first step is to create a matrix to cache the computed inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  set.inverse <- function(inverse.mat) inverse <<- inverse.mat
  get.inverse <- function() inverse
  list(set = set,
       get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}

##then when use the following function to calculate the inverse
##if the the matrix is cached, it won't be calculated
##if not, the inverse will be computed.

cacheSolve <- function(x, ...) { 
  inverse <- x$get.inverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$set.inverse(inverse)
  inverse
}
