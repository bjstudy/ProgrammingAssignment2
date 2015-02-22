## Solution for the R Programming Course, peer graded programming assignment 2
## Forked from stub at https://github.com/rdpeng/ProgrammingAssignment2
## Implementation by B. Johansen, February 2015
## 2 functions, designed to work together, to store the inverse of a matrix in a cache
## cacheSolve only works on a specially prepared vector that was set by makeCacheMatrix


## Creates a list that holds 4 functions to set or retreive the values of
## the original matrix x and the inverse matrix of it, respectively.
## Call this function first and store the returned special matrix in a placeholder in your
## work environment. After that, use the cacheSolve function to store and retrieve its inverse
## Don't call the inner functions directly as that can create inconsistency. Use cacheSolve for that
makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    get <- function() x
    
    setInverse <- function(inverseMatrix) invMatrix <<- inverseMatrix
    getInverse <- function() invMatrix

    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## NB! As requested in the assignment, the function ASSUMES that the matrix x is directly inversible
## Like the vector example it was to be based on, it also assumes that x was made by makeCacheMatrix
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
