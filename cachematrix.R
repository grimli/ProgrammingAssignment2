## These two functions permit to calculate the inverse of a given 
## matrix and to cache the result for future use.
## To use them you have to: 
## 1- define a matrix (ei. m1=matrix(c(1,0,0,0,1,0,1,0,1),3,3))
## 2- use the function makeCacheMatrix to create a special matrix object (ei. mObj = makeCacheMatrix(m1))
## 3- caclulate the inverse matrix (ei. cacheSolve(mObj))
## the first time that the step 3 is executed the inverce matrix is calculates,
## the following times the inverse matrix is get from the cache

## The makeCacheMatrix function creates a list of functions useful to manage the 
## cached value tied to the matrix get in input

makeCacheMatrix <- function(m = matrix()) {
    invM <- NULL
    set <- function(y) {
        m <<- y
        invM <<- NULL
    }
    get <- function() m
    setInv <- function(inv) invM <<- inv
    getInv <- function() invM
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## The cacheSolve function tries to retrives the inverse matrix from the cache. 
## If no cached value is available, the inverse matrix is calculated

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
