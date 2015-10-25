## Functions to allow user to calculate the inverse of a matrix
## and store it in cache and retrieve it if it has already
## been calculated and has not changed

## Function to create a special "matrix" object that
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set=set, get=get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function to retrieve the inverse of a matrix from cache
## if it has already been calculated and has not changed, otherwise
## the inverse of a matrix will be calculated and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
