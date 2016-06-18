## Put comments here that give an overall description of what your
## functions do
#We're caching the inverse of a matrix instead of computing it everytime.
#Because computing the inverse of a matrix is an expensive operation

## Write a short comment describing this function
#Steps are similiar to the given example:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
      inverse_get <- NULL
      set <- function(y) {
            x <<- y
            inverse_get <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse_set) inverse_get <<- inverse_set
      getinverse <- function() inverse_get
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse_get <- x$getinverse()
      if(!is.null(inverse_get)) {
            message("getting cached data.")
            return(inverse_get)
      }
      data <- x$get()
      inverse_get <- solve(data)
      x$setinverse(inverse_get)
      inverse_get
}
