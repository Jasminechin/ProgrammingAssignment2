## I am not very good at describing codes.
## Basically I followed the ruled in the given sample, makeVector and cachemean, to write this assignment.

## makeCacheMatrix is created to realize the inverse matrix of a given matrix. 

makeCacheMatrix <- function(x = matrix()) {
## Only the square matrix can have an inverse matrix, so x must be a square matrix.   
    inverse <- matrix()
    inverse <- NULL
## set the value of matrix x
    set <- function(y){
        x <- y
        inverse <- NULL
    }
## get the value of matrix x
    get <- function() x
## set the inverse matrix of x
    setinverse <- function(solve) inverse <<- solve
## get the inverse matrix of x, which can be obtained by the function, solve.
    getinverse <- function() inverse
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve is created to test whether the inverse matrix is calculated in the function above. If not, cacheSolve will
## then calculate the inverse matrix within itself.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    ## test if the inverse matrix is calculated already. If so, just simply call the result.
    if (!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    ## if the inverse matrix is not calculated already, get the given matrix by the get function above and calculate inverse.
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
