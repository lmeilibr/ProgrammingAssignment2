## makeCacheMatrix: 
## cacheSole: This function

## This function creates an object, with setters and getters for a Matrix and
## its inverse.
## The input argument is a 2x2 matrix

makeCacheMatrix <- function(mtx = matrix()) {
        cachedInverse <- NULL
        set <- function(newMtx){
                mtx <<- newMtx
                cachedInverse <<- NULL
        }
        get <- function() mtx
        
        ## never use this directly, only internal calculation
        setInverse <- function(solve) cachedInverse <<- solve 
        
        getInverse <- function() cachedInverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function checks if the inverse Matrix of the argument was calculate. If
## TRUE, returns the cached value, if FALSE, it calculates and return the 
## inverse matrix of the argument. The input is a object from the 
## makeCacheMatrix object.

cacheSolve <- function(mtx) {
        cached <- mtx$getInverse()
        if(!is.null(cached)) {
                message("getting cached data")
                return(cached)
        }
        data <- mtx$get()
        cachedInverse <- solve(data)
        mtx$setInverse(cachedInverse)
        cachedInverse
}
