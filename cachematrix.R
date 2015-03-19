## Program Assignment 2 of R Programming Language Course on coursera

## course id: rprog-012
## user: Lu Liu

## The functions create a special matrix that can cache its inverse.
## Use `makeCacheMatrix` to create the matrix, and cacheSolve to get 
## from cache or calcuate the inverse of the matrix 

## Construct a matrix, its inverse and their getter/setter functions

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


## Get the inverse of a matrix contained in x from cache if it has been 
## calculated before, or a fresh new calculated result

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
