## The following pair of functions help to cache the inverse of a square matrix.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setMat_Inverse <- function(Mat_Inverse) m <<- Mat_Inverse
    getMat_Inverse <- function() m
    list(set = set,
         get = get,
         setMat_Inverse = setMat_Inverse,
         getMat_Inverse = getMat_Inverse)
}


## The function cacheSolve: computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.

## For this assignment it is assumed that the matrix x supplied is always invertible.
## cacheSolve won't work if the matrix is not a square matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getMat_Inverse()
        if (!is.null(m)) {
            message("Getting Cached Data of Matrix Inverse")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setMat_Inverse(m)
        m
}
