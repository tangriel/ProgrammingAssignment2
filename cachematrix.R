## Matrix inversion is usually a costly computation.
## Thus, there may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly. 
## These functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      
      inv <- NULL
      
      ## Set/get methods for the matrix
      
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      
      ## Set/get methods for the matrix inverse
      
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      
      ## Returning the list of set/get methods
      
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
      
      inv <- x$getinv()
      
      ## Check if the inverse has already been calculated,
      ## if so, get it from the cache and skip the computation
      
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      ## Compute a matrix that is the inverse of 'x'
      
      data <- x$get()
      
      inv <- solve(data, ...)
      x$setinv(inv)
      
      ## Return a matrix that is the inverse of 'x'
      
      inv
}
