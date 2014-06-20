## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setCacheMtrx <- function(solve) m <<- solve
        getCacheMtrx <- function() m
        list(set = set, get = get,
             setCacheMtrx = setCacheMtrx,
             getCacheMtrx = getCacheMtrx)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getCacheMtrx()
		## message(m)
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
		##message("solving matrix")
        m <- solve(data, ...)
        x$setCacheMtrx(m)
        m
}

