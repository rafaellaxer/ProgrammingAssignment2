## These functions get a matrix and create a cache'd version of its inverse, so that the system 
## does not have to do the maths every time the value is asked

## This function gets a matrix as an argument, then it creates an empty value for the inverse
## It then creates the 4 functions that are gonna be used: set changes the value of the matrix,
## get gets the value of the matrix, setinv sets the value of the inverse, and get gets it

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
                }
        get <- function() x
        setinv <- function (inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function tries to get a cache'd version of the inverse of x first of all
## If the result isn't NULL, it returns that cache'd value. Otherwise, it gets the
## value of the matrix, does the inverse of it, sets that inverse in cache, and returns it.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)){
                message("Getting Cached Data")
                return(inv)
                }
        data <- x$get()
        inverse <- solve(data)
        inv <- x$setinv(inverse)
        inv
}
