## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" which contains a list of functions
## to set/get the value of the matrix and set/get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
}


## This function calculates the inverse of the special "matrix" created with the above function.
## If the inverse has already been calculated, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the inverse value via the setinverse function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
