## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function returns a list of special functions where 
## a function value is written in global environment
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## if inverse is already present, returns from cache
  i <- x$getinverse()
  
  if(!is.null(i)) {
    message("getting cached data for inverse")
    return(i)
  }
  data <- x$get()
  
  i <- solve(data)
  x$setinverse(i)
  i
}
