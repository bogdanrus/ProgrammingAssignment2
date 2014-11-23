## The following functions will cache the inverse of a matrix(assuming
   that the matrix provided is invertable)rather than computing it repeatedly.
   This will save time for computing the inverse of the matrix, every time the
   inverse is needed. 

## The makeCacheMatrix function creates a "matrix" object that can cache it's 
   inverse. The inverse of the matrix is initialy set to NULL. solve() function
   is used to calculate the inverse of the matrix. 

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


## The cacheSolve function returns the inverse of the "matrix" object computed by
   the previous function, if the matrix has not changed and the inverse was
   computed already. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
