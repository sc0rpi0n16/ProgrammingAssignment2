

# The following function enables the creation of a special matrix 
# that has extra functions like caching the inverse of itself.
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

# The following function solves for the inverse of the inputted matrix. 
# It returns the cached result if the inverse was already computed previously
cacheSolve <- function(matrix, ...) {
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
