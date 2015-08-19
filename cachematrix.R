## Together, these two functions create a special "matrix" object which can cache its 
## inverse and then compute and return the inverse, saving to and returning it from cache 
## whenever possible. This would greatly speed up calculation times, especially with 
## particularly large matrices.


## This function takes an invertible matrix as its argument and creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## cacheSolve will retrieve the inverse from the cache; if the inverse has not yet been
## calculated (or the matrix has changed), then cacheSolve will calculate the inverse using 
## the solve function and cache the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
