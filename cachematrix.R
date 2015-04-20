## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
    invM <- NULL
    set <- function(y) {
        m <<- y
        invM <<- NULL
    }
    get <- function() m
    setInv <- function(mean) invM <<- mean
    getInv <- function() invM
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(m, ...) {
    ## Return a matrix that is the inverse of 'm'
    invM <- m$getInv()
    if(!is.null(invM)) {
        message("getting cached data")
        return(invM)
    }
    data <- m$get()
    invM <- solve(data, ...)
    m$setInv(invM)
    invM
    
}
