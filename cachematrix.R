## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
    inv_mat <- NULL

    set <- function(input_mat) {
        x <<- input_mat
        inv_mat <<- NULL
    }   

    get <- function() x

    getInv <- function() inv_mat

    setInv <- function(inv) inv_mat <<- inv 

    list(set=set, get=get, getInv=getInv, setInv=setInv)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()

    # just return if already calculated
    if (!is.null(inv)) {
        message("getting cached inverse matrix.")
        return(inv)
    }   

    # otherwise, calculate, cache, then return
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInv(inv)
    inv 
}
