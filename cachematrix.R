## The following two functions provide a means to calculate iverse of a given
## matrix. Since calculating iverse of a matrix is costly, these functions provide
## a way of caching a calculated inverse to be used in all subsequent requests,
## instead of calculating for every request.

## This function creates a special type of object that stores a matrix
## and also provides setter/getter methods for the object to be used
## to store/retrieve the matrix and the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverse of a given matrix.
## Before calculating the inverse, the function tries first
## to find a cahched value. If cached value is not found, the
## function calculates the the inverse and returns the value.

cacheSolve <- function(x, ...) {
		m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
