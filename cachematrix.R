## These functions calculate and cache inverse matrix of a matrix
## 

## Return a list of functions that can cache and return a matrix 
## and calculate and cache inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  ## define function that can assign value y to x
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  ## define function that return x value
  get <- function() x
  ## define function that calculate invert matrix
  setsolve <- function(solve) s <<- solve
  ## define function that return invert matrix
  getsolve <- function() s
  ## return a list of functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Return a matrix that is the inverse of x
## if it is not calculated yet, calculate it
## if it is already calculated get it from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## get inverse matrix
  s <- x$getsolve()
  ## get value if it's already cached
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  ## if there's no cached inverse matrix, calculate the inverse matrix and cache it
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}



