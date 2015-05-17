#These two functions first create a list of functions that could be applied to an input matrix
#and then either solves for the inverse of that matrix or returns the previously cached calculation
#of that inverse.

#USAGE: The output of makeCacheMatrix() is the input argument for cacheSolve()


#This function makes the matrix and assigns 4 functions to it to set/get the value of
#the matrix, and set/get the value of the inverse
makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#Checks to see if Inverse is cached, if true returns cached value.
#Else computes inverse and caches it.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
