## Two functions to return the inverse of a matrix from cache 
## if it's already been computed
## To use:
## - Create an invertible nxn matrix called my_matrix
## - save makeCacheMatrix(my_matrix) into my_list
## - use cacheSolve(my_list) to get the inverse 
##    (first time computed, following times from cache)

## makeCacheMatrix create a list of functions to save and retrieve the result
## of the matrix inversion computation

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve performs the computation and retrieval from cache
## using the functions defined by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
