## Put comments here that give an overall description of what your
## functions do

## This function creates sort of an object (in OOP terms) representing
## a matrix, with a caching mechanism for the inverse of it. The 
## object has different methods for setting/getting its own value and
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv_mat <- NULL

    # setter for matrix value
    set <- function(input_mat) {
        x <<- input_mat
        inv_mat <<- NULL
    }   

    # getter for matrix value
    get <- function() x

    # getter for matrix's inverse
    getInv <- function() inv_mat

    # setter for matrix's inverse
    setInv <- function(inv) inv_mat <<- inv 

    # return all the methods
    list(set=set, get=get, getInv=getInv, setInv=setInv)
}

## This function takes a matrix object and solves the inverse for it.
## The result of the inverse calculation is actually cached, so that 
## it won't be re-calculated.
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
