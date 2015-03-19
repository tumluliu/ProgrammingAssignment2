## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv_m <- NULL
    set <- function(y) {
        x <<- y
        inv_m <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inv_m <<- inv
    getInverse <- function() inv_m
    list(set = set, get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_m <- x$getInverse()
    if(!is.null(inv_m)) {
        message("getting cached matrix inverse")
        return(inv_m)
    }
    data <- x$get()
    inv_m <- solve(data, ...)
    x$setInverse(inv_m)
    inv_m
}
