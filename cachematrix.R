## The functions take in an invertable matrix and output the inverse matrix
## It will cache the output value and use it for repetative command in future to save time


## This function takes in an invertable matrix and prepare a list of functions for the next function
makeCacheMatrix <- function(x = matrix()) {
    # check if matrix
    if(!is.matrix(x)) {
        stop("input is not a matrix!")
    }
    
    # check if square matrix
    if(nrow(x)!=ncol(x)) {
        stop("please input a square matrix! (equal number of rows and columns)")
    }
    
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    
    # return
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function returns the inverse matrix and save the cache if it hasn't been saved before
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse();
    if(!is.null(inverse)) {
        message("Cache found and loading");
        return(inverse)
    }
    data <- x$get();
    inverse <- solve(data,...);
    x$setInverse(inverse);
    message("saving the following data as cache")
    return(inverse)
}



