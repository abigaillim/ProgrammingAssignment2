## Creating the matrix that will be inversed and cached
## makeCacheMatrix should:
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse
##    4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
        # set the value of the matrix
            x <<- y
        # clear any previous inverse from the cache
            inv <<- NULL
        }
        # get the value of the matrix
        get <- function() x
        # set the inverse
        setInverse <- function(inverse) inv <<- inverse
        # get the inverse
        getInverse <- function() inv
        # return a list of the above 4 points
        list(set = set, get = get,
             setInverse = setInverse
             getInverse = getInverse)
}

## Retrieving the cached inverse or
## computing and caching the inverse if the cache is empty
## cacheSolve should:
##    1. Retrieve the cached inverse, OR
##    2. If the cache is empty,
##        a) compute the inverse
##        b) cache the inverse
##        c) return the inverse

cacheSolve <- function(x) {
        # retrieve the cached inverse
        inv <- x$getInverse()
        if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        # if the cache is empty,
        # calculate the inverse
        data <- x$get()
        inv <- solve(data)
        # cache the inverse
        x$setInverse(inv)
        # return the inverse
        inv
}