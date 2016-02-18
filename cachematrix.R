## The two functions makeCacheMatrix and cacheSlove can be used to create an inverse of a certain square invertible matrix

## makeCacheMatrix function creates a special 'matrix', which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix 
makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
        x <<- y
        i <<- NULL
      }
      get <- function() x
      setinverse <- function(inv) i <<- inv
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)  
}


## cacheSolve calcuates the inverse of matrix x.
## Before it really does calculation, it checks the cache value by getinverse first
## If there is a cached inversed matrix of x, print the message and just return it
## Otherwise, it will get the matrix and calculate the inverse of x (data) and set it back
## for future usage
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
