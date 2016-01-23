##Functions to cache the inverse of a matrix.

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    ## set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x  ## get the value of the matrix
    setinverse <- function(inverse) inv <<- inverse  ## set the inverse of the matrix
    getinverse <- function() inv  ##get the inverse of the matrix
    
    ##creates a special "matrix", which is really a list containing several function.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) {

    m <- x$getinverse() ## retrieve the inverse from the cache
    
    ## if already calculated the inverse of the matrix, return the inverse from the cache.
    if(!is.null(m)) { 
        message("getting cached data")
        return(m) 
    }
    
    data <- x$get() ## get the value of the matrix
    m <- solve(data, ...)  ## calculate the inverse of the matrix
    x$setinverse(m)  ## set the inverse of the matrix
    m  ## return the inverse calculate
}
