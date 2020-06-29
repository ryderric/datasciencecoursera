## makeCacheMatrix(): creates a special “matrix” object that can cache 
##          its inverse.
## cacheSolve(): computes the inverse of the “matrix” returned by 
##          makeCacheMatrix(). If the inverse has already been calculated and 
##          the matrix has not changed, it’ll retrieves the inverse from the 
##          cache directly.

## The makeCacheMatrix function generates input for the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
    
    ##  The following returns a list containing functions to:
    ##      1. set matrix
    ##      2. get matrix
    ##      3. set inverse
    ##      4. get inverse
  
    inv <- NULL
    setmat <- function(y) {
        x <<- y
        inv <<- NULL 
        }
    getmat <- function() x
    setinv <- function(inverse) inv <<- inverse 
    getinv <- function() inv
    list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)
}


## The cacheSolve function returns the inverse of the original matrix input to 
##      makeCacheMatrix()


cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  
  ## if the inverse has already been calculated get it from cache and skip 
  ##      the computation
  if (!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  
  ## else calculate the inverse 
  data <- x$getmat()
  inv <- solve(data, ...)
  
  ## set the value of the inverse in cache via setinv function.
  x$setinv(inv)
  
return(inv)
}

