## Put comments here that give an overall description of what your
## functions do

## This funtion is where the cache part is checked for new entries or existing data

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL                                 ##Initializing inverse as NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}                       ##Function to get MATRIX
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}              ##Function to obtain INVERSE of Matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
  ## If the inverse has already been calculated then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...){                ##Gets CACHE data
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){                           ##Checking if INVERSE is NULL
    message("getting cached data")
    return(inv)                                ##Returns INVERSE value
  }
  mat <- x$get()
  inv <- solve(mat, ...)                       ##Calculates INVERSE value
  x$setInverse(inv)
  inv
}
