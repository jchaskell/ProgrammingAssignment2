## These functions create a matrix object that can cache its inverse and 
## calculate the inverse, retrieving it from cache



## makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve computes the inverse of the matrix. If the inverse has already
## been calculated, then it retrieves it from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        d <- x$get()
        m <- solve(d, ...)
        x$setinverse(m)
        m
}
