## The following functions can be used to create a special
## matrix object which can compute and cache its inverse.

## This function creates a special matrix object which is 
## really just a list containing functions to set and get 
## value of the matrix as well as its mean

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special matrix 
## returned by makeCacheMatrix. The function first checks 
## whether the inverse has already been calculated, and
## whether the matrix is unchanged. If both these conditions
## are true, the matrix inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
