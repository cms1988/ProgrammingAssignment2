## This program is made to solve matrixes and store the result

## In simple cases the best option to solve a matrix is the function solve() but, sometimes we need to repeatedly solve 
## a matrix and could be a costly computation.
## The ideia of this program is to  cache the results of the calculation to save some time and computational resources.

## This program transorms a regular matrix into a "special" matrix, if you solve this "special" matrix
## with the fuction cacheSolve() it will solve the original matrix, just like the solve() function, and
## the result will be stored. If you have to solve the same "special" matrix again, it will not have 
## the process of calculating de inverse again.

## Transorming a regular matrix into a "special" matrix that will store the matrix and latter store its inverse

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Solving the matrix and storing, if it was not calculated yet

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
