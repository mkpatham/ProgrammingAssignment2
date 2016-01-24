## Caching the Inverse of a Matrix:
## The functions below are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(a = matrix()) {
        inv <- NULL
        set <- function(b) {
                a <<- b
                inv <<- NULL
        }
        get <- function() a
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. 

cacheSolve <- function(a, ...) {
        ## Return a matrix that is the inverse of 'a'
        inv <- a$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- a$get()
        inv <- solve(mat, ...)
        a$setInverse(inv)
        inv
}
