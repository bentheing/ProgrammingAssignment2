## This is a pair of functions that cache the inverse of a matrix.
##
## -----------------
## Example for usage
## -----------------
##
## 1. Create a list object with the matrix to be inverted using the
##    'makeCacheMatrix' function:
##
##    X <- makeCacheMatrix(matrix(runif(12), 3, 3))
##
## 2. Calculate and cache the inverse of matrix that is stored in 'X':
##
##    cacheSolve(X)
##
## 3. Next time you want to get the inverse matrix, it is loaded from the object
##    and not calculated again:
##
##    cacheSolve(X)


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    xInv <- NULL
    set <- function(y) {
        x <<- y
        xInv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) xInv <<- inverse
    getinverse <- function() xInv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xInv <- x$getinverse()
    if(!is.null(xInv)) {
        message("getting cached inverse matrix")
        return(xInv)
    }
    matrix <- x$get()
    message("calculate inverse matrix")
    xInv <- solve(matrix, ...)
    x$setinverse(xInv)
    xInv
}
