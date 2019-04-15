## This two functions allows the user to save time creating a cache
## for a matrix that contains its inverse matrix.

## The function below returns a list with four different functions
## to set and get the values of the matrix and its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        invmatx <- NULL
        set <- function(y) {
                x <<- y
                invmatx <<- NULL
        }
        get <- function() x
        setinv <- function(solve) invmatx <<- solve
        getinv <- function() invmatx
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## It uses the solve function to find the solution of Ax = I,
## where 'I' is the identity matrix and 'x' is the inverse matrix of A.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmatx <- x$getinv()
        if(!is.null(invmatx)){
                message("getting cached data")
                return(invmatx)
        }
        data <- x$get()
        invmatx <- solve(data, ...)
        x$setmean(invmatx)
        invmatx
}
