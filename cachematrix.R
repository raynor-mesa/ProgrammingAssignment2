## makeCacheMatrix creates a list of functions that can store a matrix and
## its inverse. cacheSolve can retrieve a stored inverse from a cacheMatrix,
## or solve for the inverse and then store it.

## Create a list of functions to store and retrieve a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        get <- function() {
                x
        }
        setcache <- function(inverse) {
                cache <<- inverse
        }
        getcache <- function() {
                cache
        }
        list(set = set, get = get, setcache = setcache, getcache = getcache)
}


## Returns a cacheMatrix's stored inverse matrix, or solves for the inverse
## if one has not been solved prior

cacheSolve <- function(x, ...) {
        cache <- x$getcache()
        if(!is.null(cache)) {
                message("Retrieving cached inverse")
                return(cache)
        }
        matrix <- x$get()
        cache <- solve(matrix, ...)
        x$setcache(cache)
        cache
        ## Return a matrix that is the inverse of 'x'
}
