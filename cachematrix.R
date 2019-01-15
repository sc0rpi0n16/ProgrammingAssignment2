## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(matrix = matrix()) {
    inv <- NULL
    set <- function(y) {
        matrix <<- y
        inv <<- NULL
    }
    get <- function() matrix
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- matrix$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- matrix$get()
    inv <- solve(data, ...)
    matrix$setinv(inv)
    inv
}
