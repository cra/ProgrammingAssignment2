## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix pseudoobject with four 'methods'
## set(x)       sets the value of a matrix
## get()        get the matrix  
## setinv(i)    set inverse matrix
## getinv()     get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL # the inverse of the 'new' matrix is NULL until 
                     # user reassigns it, e.g. via setinv() function
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv

    # declare the methods
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## returns the matrix inverse (see above for matrix pseudoobject)
## if the matrix happen to have a cached value, it is returned
## otherwise, it is computed on the fly
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data of inverse matrix")
        return(inv)
    }
    # no cached version found, compute it and save for future reference
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)

    # return the result anyway
    inv
}
