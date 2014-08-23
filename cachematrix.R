## To avoid the costly computation of matrix inversion, we aim at caching the 
## inverse of a matrix. The first function, makeCacheMatrix, creates a special
## "matrix" object that can cache its inverse. The second function, cacheSolve,
## computes the inverse of the special "matrix" returned by the first function.

## makeCacheMatrix: This function creates a special "matrix", which is really a
## @@list@@ containing 
## different functions to set the value of the matrix, get the value of the
## matrix, set the value of the inverse, get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() {
            x
      }
      setinverse <- function(inverse) {
            inv <<- mean
      }
      getinverse <- function() {
            inv
      }
      list(set = set, get = get, setinverse = setinverse, 
            getinverse = getinverse)
}

## cacheSolve: This function uses the solve function solve() to compute the 
## inverse of the special "matrix" returned by makeCacheMatrix above. If the
## inverse has already been calculated (and the matrix has not changed), then
## the cachesolve retrieves the inverse from the cache. We assume that the
## matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
