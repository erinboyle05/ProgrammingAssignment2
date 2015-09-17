## These functions will store the inverse of matrix(x)
## to a cache and call the values of the cache

## makeCacheMatrix() stores four functions:
## set() sets mat = NULL and substitutes y from input for x of main function
## get() returns vector x
## setinverse() sets mat as inverse of matrix x
## getinverse() returns mat

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y) {
                x <<- y
                mat <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) mat <<- solve
        getinverse <- function() mat
        list(set=set, get=get, setinverse=setinverse,
             getinverse=getinverse)
}


## cacheSolve() checks mat for a value and returns mat if not equal to NULL
## if mat = NULL, data is stored to makeCacheMatrix and calculates and 
## returns inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getinverse()
        if(!is.null(mat)) {
                message("getting cached data")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data, ...)
        x$setinverse(mat)
        mat
        
}
