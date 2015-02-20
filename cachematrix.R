## This pair of functions cache the inverse of a matrix. The first function 'makeCacheMatrix'
## creates a special "matrix" object that can cache its inverse. The second function 
## 'cacheSolve' computes the inverse of the special "matrix" returned by 
## 'makeCacheMatrix'. If the inverse has already been calculated (and the 
## matrix has not changed), then 'cacheSolve' retrieves the inverse from the cache.


makeCacheMatrix <- function(x) {
        # start out with empty inverse 'i'
        i <- NULL
        # 'set' takes input 'y' and puts it in 'x'; also clears inverse 'i'
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        # 'get' doesn't take an input and just returns 'x'
        get <- function()x
        # 'setinverse' takes input 'inverse' and puts it in 'i'
        setinverse <- function(inverse) i <<- inverse
        # 'getinverse' doesn't take an input and just returns 'i'
        getinverse <- function() i
        # the output of the function 'makeCacheMatrix' is a list containing 4 functions:
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
        # First check whether the inverse has been cashed
        i <- x$getinverse()
        # If it has been cashed, print message and return the cache
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        # Else get matrix from 'x$get' and put it in 'data'
        data <- x$get()
        # Calculate inverse from data and put it in 'i'
        i <- solve(data, ...)
        # Use 'i' as input in function 'x$set' to cache it
        x$setinverse(i)
        # Return 'i'
        i
}