## Two functions that can be used to cache the inverse of a matrix.

## The function makeCacheMatrix creates an object which acts on a matrix
## with a list of functions to
## (1) set the value of the matrix
## (2) get the value of the matrix
## (3) set the value of the inverse of the matrix
## (4) get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## The function cacheSolve takes an object created with the above function 
## and calculates the inverse of the related matrix. If the inverse has already 
## been calculated, it gets it from cache and skip the computation. Otherwise, 
## it gets the related matrix, computes its inverse and sets the value of the 
## inverse in the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, diag(nrow(data)), ...)
    x$setinv(inv)
    inv
}
