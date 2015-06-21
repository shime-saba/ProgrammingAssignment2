## Together, these functions allow a user to make 'matrix' objects which store
## inverse in cache, and compute (or retrieve, if already computed) that inverse.

## Makes a list containing functions to set and get the value of a matrix x
## and to set and get the value of the inverse to x (once computed).

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Calculates the inverse of a matrix object created using makeCacheMatrix.
## If inverse has already been calculated, it is returned from cache
## without duplicating computation.

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
