## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # makeCacheMatrix creates a list containing a function to
  
  # 1. set the value of the matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # 2. get the value of the matrix
  get <- function() x
  
  # 3. set the value of inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # 4. get the value of inverse of the matrix
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # This function assumes that the matrix is always invertible to return its inverse.
  
  # First check if the inverse has already been computed. 
  inv <- x$getinverse()
  if(!is.null(inv)) {
    # If so, get the result and skip the computation. 
    message("getting cached data.")
    return(inv)
  }
  
  # If not, compute the inverse.
  data <- x$get()
  inv <- solve(data)
  # Set the value in the cache.
  x$setinverse(inv)
  inv
  
}
