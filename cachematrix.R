## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix, which is a list with functions 
## to set a value of a matrix, get the value of the matrix, set the value 
## of the inverse using solve() and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function finds the inverse of the special matrix created with
## makeCacheMatrix, but first checks to see if the inverse has already 
## been calculated. If it has it uses this cached data, if not it calculates
## the inverse and caches it using setInverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
