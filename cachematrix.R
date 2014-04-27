# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the solve function in R. 
# To compute invese the function "solve" was used
# 
# It's assumed that the matrix supplied is always invertible.


# makeCacheMatrix: function, that creates a special "matrix" 
# object able to cache a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


# "cacheSolve function": returns inverse of matrix created with "makeCacheMatrix function"
# the function first checks if inverse was already calculated and if: 
# "Yes", then simply retrives the cache record;
# "No", then computes, cahes and returns the inverse matrix of 'x'
cacheSolve <- function(x, ...) {
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}