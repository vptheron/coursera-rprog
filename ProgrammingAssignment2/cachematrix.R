## The functions in this file can be used to build "cacheable matrices" providing a fast way to 
## compute a matrix inverse.

## Creates a special "matrix" as a list of four functions: 'set', 'get', 'setInverse' and 'getInverse'.
## 'getInverse' is initialized with NULL (i.e. the inverse is not computed at creation time).
makeCacheMatrix <- function(m = matrix()) {
	inv <- NULL
        set <- function(newM){
		m <<- newM
		inv <<- NULL
	}
	get <- function() m
	setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        
	list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Returns the inverse of the provided cacheMatrix. 
## This function first checks if the inverse is already cached, if it is it skips the computation and returns
## the cached value, otherwise it calculates the inverse and caches it via the 'setInverse' function of cacheM.
cacheSolve <- function(cacheM, ...) {
        inv <- cacheM$getInverse()
        if(is.null(inv)){
		message("Computing inverse and caching result")
		inv <- solve(cacheM$get(), ...)
                cacheM$setInverse(inv)
	} else {
		# This block could be removed, but the message helps understanding what is happening.
		message("Getting cached inverse")
	}
	inv
}
