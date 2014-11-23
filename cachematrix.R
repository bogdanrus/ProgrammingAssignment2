## The following functions will cache the inverse of a matrix(assuming
   that the matrix provided is invertable)rather than computing it repeatedly.
   This will save time for computing the inverse of the matrix, every time the
   inverse is needed. 

    ## The makeCacheMatrix function creates a "matrix" object that can cache it's 
       inverse. The inverse of the matrix is initialy set to NULL. solve() function
       is used to calculate the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {           ## input "x" will be an invertable matrix
        m <- NULL                                     ## m is the inverse of "x", set to NULL every time the function is called 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x                           ## returns the value of the original matrix
        setinverse <- function(solve) m <<- solve     ## called by first cacheSolve() and stores the value using "<<-"
        getinverse <- function() m                    ## returns the cached value to cacheSolve()
        list(set = set, get = get,                    ## accessed each time a new matrix is created, each time makeCacheMatrix() is called
             setinverse = setinverse,
             getinverse = getinverse)

}


    ## The cacheSolve function returns the inverse of the "matrix" object computed by
       the previous function, if the matrix has not changed and the inverse was
       computed already. 

cacheSolve <- function(x, ...) {                      ## input "x" is an object created by makeCacheMatrix()
        m <- x$getinverse()                           ## accesses object "x" and gets the matrix inverse
        if(!is.null(m)) {                             ## if inverse is not NULL, a message is sent
                message("getting cached data")        ## message sent
                return(m)                             ## returns the inverse of the matrix and ends cacheSolve()
        }
        data <- x$get()                               ## executed only if x$getinverse() is NULL
        m <- solve(data, ...)                         ## if the inverse (m) is NULL, we calvulate the new inverse
        x$setinverse(m)                               ## stores the calculated inverse
        m                                             ## inverse of matrix
        ## Return a matrix that is the inverse of 'x'
}
