## These functions will store the inverse of matrix(x)
## to a cache and call the values of the cache

## makeCacheMatrix() stores four functions:
## set() sets m=NULL and substitutes y from input for x of main function
## get() returns vector x
## setinverse() sets m as inverse of matrix x
## getinverse() returns m

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set=set, get=get, setinverse=setinverse,
             getinverse=getinverse)
}


## cacheSolve() checks m for a value and returns m if not equal to NULL
## if m=NULL, data is stored to makeCacheMatrix and calculates and 
## returns inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}
