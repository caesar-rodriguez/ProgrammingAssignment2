## Put comments here that give an overall description of what your
## functions do
######This section is part of the Programming Assigment of Coursera R Programming
#######Programming assigment 2: Lexical Scoping
###########By: César Ricardo Rodríguez Luna

## Write a short comment describing this function
####This function a special matrix object that can cache its inverse.



makeCacheMatrix <- function(x = matrix()) { ## define the argument as a matrix by default
    inv <- NULL                             ## will hold value of matrix inverse
    set <- function (y) {                   ## set function to assign new matrix in environment
        x <<- y
        inv <<- NULL                        ##if there is a new matrix, then take inv to NULL
    }
    get <- function () x                    ## returns value of the matrix argument
    setinverse <- function(inverse) inv <<- inverse       ## assigns value of inv in environment
    getinverse <- function() inv                          ## gets the value of inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)           ##name for reference
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {            ## dReturn de inverse matrix of "x"
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <-x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
