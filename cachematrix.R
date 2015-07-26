## This code will write a pair of functions that cache the inverse of a matrix.

## The first function is 'makeCacheMatrix' which creates a special "matrix" object that 
## can cache its inverse.

## The second function is 'cacheSolve' which computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(y) {
                x <<- y
                invrs <<- NULL
                
        }
        get <- function() x
        setInverse <- function(inverse) invrs <<- inverse
        getInverse <- function() invrs
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## 'cacheSolve' which computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        invrs <- x$getInverse()
        if (!is.null(invrs)) {
                message("getting cached data")
                return(invrs)
        }
        mat <- x$get()
        invrs <- solve(mat, ...)
        x$setInverse(invrs)
        invrs
}