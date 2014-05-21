
##Description of the functions:
##The code calculates the inverse of a matrix, if it has not already been calculated. 
##If you encounter this last case, the function returns the value from the cache.


##The makeCacheMatrix, firstly, creates special object that stores a matrix 
## and cache its inverse. 
##Set and get the value of the matrix
##Set and get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
##The cacheSolve calculates the inverse of the matrix created before
## and checks to see if the inverse has already been calculated. 
# If so, it returns the inverse from the cache. Otherwise, it calculates the inverse.
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}