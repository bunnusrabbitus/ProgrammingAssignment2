## These two functions makeCacheMatrix and cacheSolve are used together in the form cacheSolve(makeCacheMatrix(x)) where x is a square matrix. The functions return the inverse matrix of the input square matrix x

## makeCacheMatrix takes as its argument a square matrix and returns a list of functions named set, get, set.inverse, and get.inverse. These functions are cached and can be accessed by the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        set.inverse <- function(solve) i <<- solve
        get.inverse <- function() i
        list(set = set, get = get,
             set.inverse = set.inverse,
             get.inverse = get.inverse)
}


## cacheSolve evaluates the value of x$get.inverse() if it isn't null it returns the inverse of x that was assigned to variable i in makeCacheMatrix, if the inverse of matrix x was not cached, it solves for x using the cached matrix x from makeCacheMatrix

cacheSolve <- function(x, ...) {
        
        i <- x$get.inverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$set.inverse(i)
        i
        ## Returns a matrix that is the inverse of 'x'
}

