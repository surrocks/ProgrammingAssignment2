########################################################################
#Week 3 - Prog Asgmt 2
#makeCacheMatrix: This function creates a special "matrix" object that 
#can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix"
#returned by makeCacheMatrix above. If the inverse has already been 
#calculated (and the matrix has not changed), then the cachesolve 
#should retrieve the inverse from the cache.
########################################################################

## Function to create an object that has components to:
## a. set and retrieve value of a matrix
## b. set and retrieve the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Function to calculate/return (if already calculated) the inverse
## of a matrix.
## Assumption: Matrix is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
