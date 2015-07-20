##The first function take matrix
##set the value of the matrix
##get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
  x <<- y
  inv <<- NULL
}
get <- function() x
setinv <- function(inverse) inv <<- inverse 
getinv <- function() inv
list(set=set, get=get, setinv=setinv, getinv=getinv)
}

# Returns the inverse of the matrix. It first checks if the inverse has already been 
#cached If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  data <- x$get()
  inv = solve(data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
  
}
