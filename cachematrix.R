## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "vector", which is really a list containing a function to
## set and get matrix, and set and get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    # set the maxtix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    # In this example we introduce the <<- operator which can be used to assign a value to an object in an environment that is different from the current environment. 
    # get the matrix
    get <- function() x
    
    # set the inverse of the matrix
    setinverse <- function(new_inverse) inverse <<- new_inverse
    
    # get the inverse of the matrix
    getinverse <- function() inverse
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## calculates the inverse of matrix.  However, it first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the cache
## and skips the computation. 
## Otherwise, it calculates the inverse of matrix  
## and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## it first checks to see if the
        ## inverse has already been calculated. If so, it gets the inverse from the cache
        ## and skips the computation. 
        inverse <- x$getinverse()
        if(! is.null(inverse)) {
            message("get cached data")
            return(inverse)
        }

        ## Otherwise, it calculates the inverse of matrix  
        ## and sets the value of the inverse in the cache via the setinverse function.
        data <- x$get()
        inverse <- solve(x, ...)
        x$setinverse(inverse)
        inverse
}
