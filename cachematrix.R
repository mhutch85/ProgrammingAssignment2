## This pair of functions (makeCacheMatrix and cacheSolve) allows the 
## user to cache the result of a matrix inverse calculation through 
## the use of a matrix object with get and set methods.

## Create a CacheMatrix object with properties to store the matrix 
## and its inverse and methods to get/set the properties.
makeCacheMatrix <- function(x = matrix()) {
     
     ## Create 'inv' to hold matrix inverse, set to NULL
     inv <- NULL
     
     ## Create set function that sets value of matrix and clears any 
     ## existing inverse
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }

     ## Create get function that returns the value of the matrix
     get <- function() {
          x
     }
     
     ## Create setinv function that sets the matrix inverse to the 
     ## result of the inverse calculation (calculate by cacheSolve)
     setinv <- function(inv_result) {
          inv <<- inv_result
     }
     
     ## Create getinv function that returns the inverse of the matrix
     getinv <- function() {
          inv
     }
     
     ## Return a list of the set, get, setinv, and getinv functions
     list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Return the inverse of the CacheMatrix object 'x' by retrieving the 
## cached value if it is non-null. If the cached value is null, 
## calculate the matrix inverse, store it in cache, and return it.
cacheSolve <- function(x, ...) {
     
     ## Use the getinv function on the matrix to get the matrix 
     ## inverse if it has been cached
     inv <- x$getinv()
     
     ## Alert the user and return the cached inverse if it is non-null
     if(!is.null(inv)) {
          message("Getting cached inverse")
          return(inv)
     }
     
     ## If the function has not yet returned, the cached inverse is 
     ## null. Get the matrix data to calculate its inverse.
     mdata <- x$get()
     
     ## Calculate the matrix inverse passing along any '...' arguments
     inv <- solve(mdata, ...)
     
     ## Call setinv to cache the result of the matrix inverse
     x$setinv(inv)
     
     ## Return the calculated matrix inverse
     inv
     
}
