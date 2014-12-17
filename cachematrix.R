## Caching the Inverse of a matrix
##

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(inputX){
        x <- inputX
        inv <- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set=set, get=get,
         setInv=setInv,
         getInv=getInv)
}

## This funciton calculates the inverse of the special matrix
## created with the above function. However, it first checks
## to see if the inverse has already been calculated. 
## If so, it 'get's the mean from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the 
## matrix and set the value of the inverse in the cache via
## the 'setInv' function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)){
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
