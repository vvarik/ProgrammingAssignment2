## Set up inverse matrix storage and retrieval in 
## cache during first and subsequent calls, respectively

## Creates a list of functions necessary to store and retrieve
## inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- matrix()
        set <- function(y) {
                x <<- y
                m <<- matrix()
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Returns an inverse matrix on first call, using function 'solve'
## Retrieves inverse matrix from cache during subsequent calls

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(all(!is.na(m))) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
