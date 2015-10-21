## cachematrix.R
## this file contains 2 functions
## the first constructs a special matrix that caches its own inverse
## it is called like this:
## 
## myspecialmatrix <- makeCacheMatrix(x) # where x is an invertible matrix
## cacheSolve(myspecialmatrix)


## makeCacheMatrix
## constructs a vector that contains 4 functions 
## 2 to set and get the input matrix
## and 2 to setinverse and getinverse
## this is stored in the global environment

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    
    ## the following is returned
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## cacheSolve
## receives the special matrix constructed by makeCacheMatrix
## returns the inverse, whether from the cache 
## or if not yet calculated, is calculated then cached and  returned

cacheSolve <- function(x, ...) {
    
    ## x is a special matrix constructed by makeCacheMatrix
    
    ## call the getinverse function within the special matrix
    m <- x$getinverse()
    
    # check to see if the inverse has  already been calculated
    if(!is.null(m)) {
        # in this case it has already been calculated so we can exit
        message("getting cached data")
        return(m)
    }
    
    # if we get here we know we do not yet have the inverse
    # fetch our matrix using the get function within the special matrix
    data <- x$get()
    
    # calculate the inverse using the built-in function
    m <- solve(data, ...)
    
    # call the setinversefunction within the special matrix, thereby  caching the result for next time
    x$setinverse(m)
    
    ## the newly calculated inverse
    m         
    
}
