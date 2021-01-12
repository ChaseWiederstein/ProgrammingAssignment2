## The set of these functions create a special "matrix" object, caches its
## inverse, and returns the inverse.

##used to plug into functions as an example
myMatrix <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol =2))

## The makeCacheMatrix creates a matrix object and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
m <- NULL ## the variable where the inverse is stored
set <- function(y){ 
  x <<- y
  m <<- NULL
}
get <- function() x ## retrieves the matrix
setMatrix <- function(x) {
  m <<- solve(x)
}
getMatrix <- function() {
  m
}
list(set = set, get = get,
     setMatrix = setMatrix,
     getMatrix = getMatrix)
}

## The cacheSolve function finds the inverse of the matrix object created in
## in makeCacheMatrix and tests if it has already been found. If it has been 
## found, then it searches through the cache and retrieves it. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrix()
  if (!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) ## sets m equal to the inverse of x after retrieving it
  x$setMatrix(m) 
  m
}
