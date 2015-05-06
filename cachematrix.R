# Cache the inverse of a matrix (square matrix - n by n)

# Function that create a list with 4 functions:
# 1 . Set a matrix
# 2 . Get a matrix
# 3 . Set the matrix inversion
# 4 . Get the matrix inversion

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
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


# Function to calculate the inverse of matrix retuned by makeCacheMatrix
# This matrix needs to be a square matrix (n by n)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse mtxof 'x'
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
