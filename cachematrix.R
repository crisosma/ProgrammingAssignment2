## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#creates  special matrix that is actually the following list
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inver) inv <<- inver
    getinv <- function() inv
    list(
        set = set, 
        get = get,
        setinv = setinv,
        getinv = getinv
    )
}


## Write a short comment describing this function
#it computes the inverse of x, 
#if it is computed before it returns the cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}


##testing the functions

matrix2x2 <- matrix(1:4,2,2)
matrix2x2
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
matrix2x2_esp <- makeCacheMatrix(matrix2x2)
cacheSolve(matrix2x2_esp)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
cacheSolve(matrix2x2_esp)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
