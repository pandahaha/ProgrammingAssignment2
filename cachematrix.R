## This function is used to caching the inverse of a matrix.Matrix inversion is usally
## time consumming .By caching the inverse of the matrix, we can retrive the data from 
## cache when we need it ,so that repeatd calculation can be avoided.

## This funciton is ued to create matrix and cache its inverse value.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the matrix created above.If the inverse has exist
## it will retrive the data directly from the cache.Otherwise, it will calculate and cache it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}