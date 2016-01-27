####  R Programming (JHU)    -- LightWorks Analytics -- UK

## Function for Caching the Inverse of a Matrix:
## Matrix inversion can be a costly computation and there will be some benefit
## to caching the inverse of a matrix rather than use unneccessary resource

## Below are the functions that are used to create a global object that 
## stores (globally) a matrix and caches its inverse.

## This function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
## This function uses the matrix created from the function: - makeCacheMatric above  
## and inverses it. If the inverse has been previously calculated (and the 
## matrix has not updated), then it should get the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
