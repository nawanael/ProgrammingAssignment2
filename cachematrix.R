## To avoid the costly computation of matrix inversion, we aim at caching the 
## inverse of a matrix. The first function, makeCacheMatrix, creates a special
## "matrix" object that can cache its inverse. The second function, cacheSolve,
## computes the inverse of the special "matrix" returned by the first function.

## makeCacheMatrix: This function creates a special "matrix", which is really a
## list containing different functions to set the value of the matrix, get the
## value of the matrix, set the value of the inverse, get the value of the
## inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL ## initialize base value
      set <- function(y) {
            x <<- y
            inv <<- NULL
      } ## sets the value of the matrix 'x' in cache
      get <- function() {
            x
      } ## retrieves the value of matrix 'x' in cache
      setinverse <- function(inverse) {
            inv <<- inverse
      } ## sets the value of 'inv' (inverse of matrix 'x') in cache
      getinverse <- function() {
            inv
      } ## retrieves the value of 'inv' (inverse of matrix 'x') in cache
      list(set = set, get = get, setinverse = setinverse, 
            getinverse = getinverse)
      ## returns a list of the set and get methods
}

## cacheSolve: This function uses the solve function to compute the 
## inverse of the special "matrix" returned by makeCacheMatrix above. If the
## inverse has already been calculated (and the matrix has not changed), then
## the cachesolve retrieves the inverse from the cache. We assume that the
## matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
## Returns a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        } ## if value is not null, the inverse has already been computed, so
	  ## the function retrieves the value from cache
	## if value is null, the function computes the inverse of matrix 'x':
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
