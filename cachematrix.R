## These functions calculate the inverse of a matrix and store the result in a
## "special" matrix called m at a higher level

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL    ## empty the "special" matrix m
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(In) m <<- In
    getinverse <- function() m  ## return "special" matrix m
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()   ## get "special" matrix m from higher level.
    if(!is.null(m)) {     ## if the "special" matrix has a content, return this content
        message("getting cached data")
        return(m)
    }
    data <- x$get()       
    m <- solve(data, ...)
    x$setinverse(m)        ## write the content into the "special" matrix.
    m
}

