## The following functions work together to create a square invertible matrix
## and make the inverse of the matrix available in the cache environment

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<-y
                m <<- NULL
        }
        get <- function() x
        setMatrix <- function(solve) m <<- solve
        getInverse <- function() m
        
        list(set = set, get = get,
             setMatrix = setMatrix,
             getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setMatrix(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
