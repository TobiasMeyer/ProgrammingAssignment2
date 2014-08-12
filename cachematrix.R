## Created by Tobias Meyer for Coursera -> R Programming -> Assignment 2
##
## The provided functions allow to work with cached matrix inverses, i.e. 
## cache the inverse of a matrix after it's first calculation until the matrix gets
## changed, and provide access to the matrix and it's inverse.
##
## Please note, that a 'matrix' created using makeCacheMatrix is not an actual matrix,
## but a list containing methods for access of the matrix stored internally.


## Creates an object with matrix-like functionality. Access using provided methods, e.g.
## through obj$get() to get the internally stored matrix.
makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    set <- function(mat) {
        x <<- mat
        i <<- NULL
    }

    get <- function() x  
    setInv <- function(inv) i <<- inv
    getInv <- function() i
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)   
}


## Gets inverted matrix of provided matrix-like object. This is done either through
## calculating the inverse using solve() or through accessing cached inverse, if 
## such is already stored.
cacheSolve <- function(x, ...) {
    i <- NULL
    if (is.null(x$getInv())) {
        message('Calculating inverse...')
        i <- solve(x$get())
        x$setInv(i)
    } else {
        message('Getting cached inverse...')
        i <- x$getInv()
    }
    i
}