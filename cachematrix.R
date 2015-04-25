## This script provides functions for cache operations on matrices  
## Using the functions below the following caching operations can be performed:
## 1. Write a matrix to cache
## 2. Read a matrix from cache
## 3. Calculate the inverse of a cached matrix and write the result to cache
## 4. Read the inverse of a matrix from cache

## The function makeCacheMatrix can be used to perform operations: 1, 2 and 4

makeCacheMatrix <- function(x = matrix()) {
        # initialize the inverse  
        inv <- NULL
        
        # write a matrix and initialize the inverse
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # read from cache function the input matrix
        get <- function() x
        
        # write, read functions for the inverse matrix
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        
        # return a list that can be reused to call the functions defined within makeCacheMatrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The function cacheSolve can be used to perform operations: 3, 4

cacheSolve <- function(x, ...) {
        
        # read the inverse of the input matrix from cache
        inv <- x$getinv()
        
        # if the inverse is found in the cache, return the value
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # get the cached matrix and calculate the inverse
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        
        # return the calucated inverse matrix
        inv        
}
