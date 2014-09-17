##You can find my code below:


##Part 1. This part set the value of the matrix, get the value of the matrix, 
##set the value of the solve function and get the value of the solve function

makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
a <- makeCacheMatrix(matrix(c(4,3,3,2), nrow=2, ncol=2))


##Part 2. This part calculated the inverse matrix (solve function) of the matrix created with the function
##you can find above. If 1st checks to see if the inverse matrix has already been calculated. If so, it gets the inverse
##matrix from the cache and skips the computation. Otherwise, it calculated the inverse matrix of the data
##and sets the value of the inverse matrix in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

cacheSolve(a)
