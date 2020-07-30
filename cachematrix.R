## Put comments here that give an overall description of what your
## functions do

## This funtion is where the cache part is checked for new entries or existing data

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
  ## If the inverse has already been calculated then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...){
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
